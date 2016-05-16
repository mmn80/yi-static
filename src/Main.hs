{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative    ((<|>))
import           Control.Monad          (when)
import           Control.Monad.State    (get, put, gets)
import           Control.Monad.State    (execStateT)
import           Control.Lens           ((^.), use)
import           Data.List              (intersperse)
import           Data.Char              (ord)
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import qualified Data.List.PointedList  as PL
import           Numeric                (showHex)
import           System.Console.Docopt
import           System.Environment     (getArgs)
import           Yi
import qualified Yi.Rope                as R
import           Yi.Command             (shellCommandE)
import           Yi.Config.Simple.Types (ConfigM (..))
import qualified Yi.Keymap.Emacs        as E
import           Yi.Keymap.Emacs.Utils  (findFileNewTab)
import           Yi.Mode.Haskell
import           Yi.Command             (searchSources)
import           Yi.Hoogle              (hoogleRaw)
import           Yi.TextCompletion      (wordComplete)
import           Yi.Utils               (io)
import           Yi.Tab                 (tabLayoutManagerA, tabLayout)
import           Yi.Layout              (LayoutManager(..))

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
  }

myKeymapSet :: KeymapSet
myKeymapSet = E.mkKeymap $ E.defKeymap `override` \sup _ ->
  sup { E._eKeymap = overKeymap <|| E._eKeymap sup <|> myKeymap }

overKeymap :: Keymap
overKeymap = choice [ spec KEnter       ?>>! doEnter
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
                  , char 'l'              ?>>! layoutDescribe
                  , ctrlCh 'l'            ?>>! layoutNext
                  , char '.'              ?>>! layoutVariantNext
                  , char ','              ?>>! layoutVariantPrev
                  ]
        layoutNext        = layoutManagersNextE >> layoutDescribe
        layoutVariantNext = layoutManagerNextVariantE >> layoutDescribe
        layoutVariantPrev = layoutManagerPreviousVariantE >> layoutDescribe

layoutDescribe :: EditorM ()
layoutDescribe = do
  e <- get
  let l  = tabLayout $ e ^. tabsA . PL.focus
  let lm = e ^. tabsA . PL.focus . tabLayoutManagerA
  printMsgs $ T.pack <$> [ describeLayout lm, show l ]

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
                     Just cn -> T.pack $ R.unCn cn
      let pct | pos == 0 || s == 0 = " Top"
              | pos == s = " Bot"
              | otherwise = getPercent p s
          changed   = if unchanged then "-" else "*"
          readOnly' = if ro then "%" else changed
          hexxed    = T.pack $ showHex (ord curChar) ""
          hexChar   = "0x" <> T.justifyRight 2 '0' hexxed
          toT       = T.pack . show
      nm <- gets $ shortIdentString (length prefix)
      return $ T.concat [ enc, " ", readOnly', changed, " ", nm, "    "
                        , pct, " ", hexChar, " ", T.justifyLeft 9 ' ' $
                            "(" <> toT ln <> "," <> toT col <> ")"
                        , "    ", modeNm
                        ]
