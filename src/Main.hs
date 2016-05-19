{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative    ((<|>))
import           Control.Monad          (when, join)
import           Control.Monad.State    (get, put, gets)
import           Control.Monad.State    (execStateT)
import           Control.Lens           ((^.), (%=), to, use, uses, assign)
import           Data.List              (intersperse, find)
import           Data.List.NonEmpty     (NonEmpty ((:|)))
import           Data.Char              (ord)
import           Data.Monoid            ((<>))
import           Data.Maybe             (isJust)
import qualified Data.Text              as T
import qualified Data.List.PointedList  as PL
import           Numeric                (showHex)
import           System.Console.Docopt
import           System.Environment     (getArgs)
import           System.Hclip
import           Yi
import qualified Yi.Rope                as R
import           Yi.Command             (shellCommandE, searchSources)
import           Yi.Config.Simple.Types (ConfigM (..))
import qualified Yi.Keymap.Emacs        as E
import           Yi.Keymap.Emacs.Utils  (findFileNewTab)
import           Yi.Mode.Haskell
import           Yi.Hoogle              (hoogleRaw)
import           Yi.TextCompletion      (wordComplete)
import           Yi.Utils               (io)
import           Yi.Layout              (Layout(..))
import           Yi.Tab                 (tabLayout)
import           Yi.KillRing            (Killring (_krContents), krPut)

help :: Docopt
help = [docopt|
    Usage:
      yi [<file> ...]
      yi (-h|--help)

    Options:
      -h, --help   Show usage
|]

main :: IO ()
main = do
  args <- parseArgsOrExit help =<< getArgs
  when (args `isPresent` (longOption "help") || args `isPresent` (shortOption 'h')) $
    exitWithUsage help
  let files = getAllArgs args (argument "file")
      actions = intersperse (EditorA newTabE) (map (YiA . openNewFile) files)
  cfg <- execStateT (runConfigM publish) (myConfig actions)
  startEditor cfg Nothing

publish :: ConfigM ()
publish = do
  publishAction "shellCommandE"             shellCommandE
  publishAction "cd"                        cd
  publishAction "pwd"                       pwd
  publishAction "searchSources"             searchSources
  publishAction "nextWinE"                  nextWinE
  publishAction "acceptedInputsOtherWindow" acceptedInputsOtherWindow
  publishAction "wordComplete"              wordComplete
  publishAction "ghciSend"                  ghciSend
  publishAction "ghciLoadBuffer"            ghciLoadBuffer
  publishAction "ghciInferType"             ghciInferType
  publishAction "ghciSetProcessName"        ghciSetProcessName
  publishAction "ghciSetProcessArgs"        ghciSetProcessArgs
  publishAction "hoogle"                    hoogle
  publishAction "hoogleSearch"              hoogleSearch

myConfig :: [Action] -> Config
myConfig actions = defaultEmacsConfig
  { modeTable = fmap (configureIndent . configureModeline) (modeTable defaultEmacsConfig)
  , defaultKm = myKeymapSet
  , configCheckExternalChangesObsessively = False
  , startActions =
      (EditorA (do
        e <- get
        put e { maxStatusHeight = 30 }))
    : actions
  , configRegionStyle = Exclusive
  , configUI = let cui = configUI defaultEmacsConfig
               in cui { configTheme = updateTheme $ configTheme cui }
  }

updateTheme :: Theme -> Theme
updateTheme t = t `override` \sup _ ->
  sup { modelineAttributes = (modelineAttributes sup) { foreground = lightGrey }}

myKeymapSet :: KeymapSet
myKeymapSet = E.mkKeymap $ E.defKeymap `override` \sup _ ->
  sup { E._eKeymap = overKeymap <|| E._eKeymap sup <|> myKeymap }

overKeymap :: Keymap
overKeymap = choice [ spec KEnter       ?>>! doEnter
                    , ctrlCh 'w'        ?>>! doCut
                    , metaCh 'w'        ?>>! doCopy
                    , ctrlCh 'y'        ?>>! doPaste
                    , spec KTab         ?>>! doTab IncreaseCycle
                    , shift (spec KTab) ?>>! doTab DecreaseCycle
                    ]
  where doTab :: IndentBehaviour -> EditorM ()
        doTab b = withCurrentBuffer $ do
                    r <- getSelectRegionB
                    if regionIsEmpty r then adjIndent b
                    else let d = if b == IncreaseCycle then 1 else -1
                         in shiftIndentOfRegionB d r
        doEnter = newlineB >> adjIndent IncreaseCycle

doCut :: YiM ()
doCut = do
  text <- withCurrentBuffer $ do
    r <- getSelectRegionB
    text <- readRegionB r
    if regionStart r == regionEnd r then bkillWordB else deleteRegionB r
    return text
  io . setClipboard $ R.toString text

killringPut :: Direction -> R.YiString -> EditorM ()
killringPut dir s = killringA %= krPut dir s

doCopy :: YiM ()
doCopy = do
  (r, text) <- withCurrentBuffer $ do
    r <- getSelectRegionB
    text <- readRegionB r
    assign highlightSelectionA False
    return (r, text)
  withEditor $ killringPut (regionDirection r) text
  io . setClipboard $ R.toString text

doPaste :: YiM ()
doPaste = do
  t <- io getClipboard
  let text = R.fromString t
  withEditor $ do
    text' :| _ <- uses killringA _krContents
    when (text' /= text) $ killringPut Forward text
    withCurrentBuffer $ pointB >>= setSelectionMarkPointB >> insertN text

myKeymap :: Keymap
myKeymap = choice [ ctrl (spec KPageDown) ?>>! previousTabE
                  , ctrl (spec KPageUp)   ?>>! nextTabE
                  , meta (spec KDown)     ?>>! nextWinE
                  , metaCh 's'            ?>>! searchSources
                  , metaCh '`'            ?>>! shellCommandE
                  , ctrl (metaCh 'a')     ?>>! wordComplete
                  , ctrlCh 'x'            ?>>  ctrlX
                  ]
  where ctrlX = choice
                  [ char 'f'              ?>>! findFileNewTab
                  , ctrlCh 'd'            ?>>! deleteTabE
                  , ctrlCh 'k'            ?>>! closeBufferAndWindowE
                  , ctrlCh 'p'            ?>>! hoogle
                  , char 'l'              ?>>! layoutManagersPrintMsgE
                  , ctrlCh 'l'            ?>>! layoutManagersNextE
                  , char '.'              ?>>! layoutManagerNextVariantE
                  , char ','              ?>>! layoutManagerPreviousVariantE
                  , char '-'              ?>>! moveDivider False
                  , char '='              ?>>! moveDivider True
                  ]

moveDivider :: Bool -> EditorM ()
moveDivider dir = do
  e <- get
  let l = e ^. tabsA . PL.focus . to tabLayout
      mbr = findFirstDivider l
      findFirstDivider (Pair _ p r _ _) = Just (r, p)
      findFirstDivider (Stack _ ws) = join . find (isJust)
                                    . map (findFirstDivider . fst) $ ws
      findFirstDivider _ = Nothing
      clamp = min 0.9 . max 0.1
  maybe (return ()) 
        (\(r, p) -> setDividerPosE r . clamp $ p + if dir then 0.2 else (-0.2))
        mbr
  
hoogle :: YiM ()
hoogle = do
  word <- withCurrentBuffer $ do
    wordRegion <- regionOfB unitWord
    readRegionB wordRegion
  hoogleSearch word

hoogleSearch :: R.YiString -> YiM ()
hoogleSearch src = do
  results <- io $ hoogleRaw src R.empty
  let r = T.break (== ' ') . R.toText <$> results
  let mx = maximum $ T.length . fst <$> r
  let format (p, s) = (if p == "Did" then p
                       else T.justifyLeft mx ' ' p)
                      `T.append` s
  printMsgs $ map format r

configureIndent :: AnyMode -> AnyMode
configureIndent = onMode $ \m ->
  m { modeIndentSettings = IndentSettings { expandTabs = True
                                          , shiftWidth = 2
                                          , tabSize    = 2
                                          }
    }

configureModeline :: AnyMode -> AnyMode
configureModeline = onMode $ \m -> m { modeModeLine = myModeLine }
  where
    myModeLine prefix = do
      col       <- curCol
      pos       <- pointB
      ln        <- curLn
      p         <- pointB
      s         <- sizeB
      curChar   <- readB
      ro        <-use readOnlyA
      modeNm    <- gets (withMode0 modeName)
      unchanged <- gets isUnchangedBuffer
      enc       <- use encodingConverterNameA >>= return . \case
                     Nothing -> mempty
                     Just cn -> T.pack $ case R.unCn cn of
                                           "UTF-8" -> "U"
                                           other   -> other
      let pct | pos == 0 || s == 0 = " Top"
              | pos == s = " Bot"
              | otherwise = getPercent p s
          changed   = if unchanged then "-" else "*"
          readOnly' = if ro then "%" else changed
          hexxed    = T.pack $ showHex (ord curChar) ""
          hexChar   = "0x" <> T.justifyRight 2 '0' hexxed
          toT       = T.pack . show
      nm <- gets $ shortIdentString (length prefix)
      return $ T.concat [ enc, readOnly', changed, " ", nm, "    "
                        , pct, " ", hexChar, " ", T.justifyLeft 9 ' ' $
                            "(" <> toT ln <> "," <> toT col <> ")"
                        , "  ", modeNm
                        ]
