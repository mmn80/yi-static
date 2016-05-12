{-# LANGUAGE QuasiQuotes #-}

import Control.Monad.State   (get, put)
import Control.Monad         (when)
import Data.List             (intersperse)
import System.Console.Docopt
import System.Environment    (getArgs)
import Yi

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
  startEditor (myConfig actions) Nothing

myConfig :: [Action] -> Config
myConfig actions = defaultEmacsConfig
  { configCheckExternalChangesObsessively = False
  , startActions =
      (EditorA (do
        e <- get
        put e { maxStatusHeight = 30 }))
    : actions
  }
