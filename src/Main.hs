{-# LANGUAGE QuasiQuotes #-}

import           Control.Applicative    ((<|>))
import           Control.Monad          (when)
import           Control.Monad.State    (get, put)
import           Control.Monad.State    (execStateT)
import           Data.List              (intersperse)
import           System.Console.Docopt
import           System.Environment     (getArgs)
import           Yi
import           Yi.Command             (shellCommandE)
import           Yi.Config.Simple.Types (ConfigM (..))
import qualified Yi.Keymap.Emacs        as E
import           Yi.Keymap.Emacs.Utils  (findFileNewTab)
import           Yi.Mode.Haskell

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
  publishAction "shellCommandE"      shellCommandE
  publishAction "ghciSend"           ghciSend
  publishAction "ghciLoadBuffer"     ghciLoadBuffer
  publishAction "ghciInferType"      ghciInferType
  publishAction "ghciSetProcessName" ghciSetProcessName
  publishAction "ghciSetProcessArgs" ghciSetProcessArgs

myConfig :: [Action] -> Config
myConfig actions = defaultEmacsConfig
  { defaultKm = myKeymapSet
  , configCheckExternalChangesObsessively = False
  , startActions =
      (EditorA (do
        e <- get
        put e { maxStatusHeight = 30 }))
    : actions
  }

myKeymapSet :: KeymapSet
myKeymapSet = E.mkKeymap $ E.defKeymap `override` \sup _ ->
  sup { E._eKeymap = E._eKeymap sup <|> myKeymap }

myKeymap :: Keymap
myKeymap = choice [ ctrl (spec KPageDown) ?>>! previousTabE
                  , ctrl (spec KPageUp)   ?>>! nextTabE
                  , ctrlCh 'x'            ?>>  ctrlX
                  ]
  where ctrlX = choice
                  [ char 'f'              ?>>! findFileNewTab
                  , ctrlCh 'd'            ?>>! deleteTabE
                  ]
