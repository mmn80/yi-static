import Yi

main :: IO ()
main = do
--    args <- parseArgsOrExit help =<< getArgs
--    let files = getAllArgs args (argument "file")
--        actions = intersperse (EditorA newTabE) (map (YiA . openNewFile) files)
    startEditor (myConfig {-actions-}) Nothing

myConfig :: {-[Action] ->-} Config
myConfig {-actions-} = defaultEmacsConfig
    -- { modeTable =
    --     fmap
    --         (configureModeline . configureIndent)
    --         (myModes defaultVimConfig)
    -- , defaultKm = myKeymapSet
    -- , configCheckExternalChangesObsessively = False
    -- , startActions =
    --     (EditorA (do
    --         e <- get
    --         put e { maxStatusHeight = 30 }))
    --     : YiA guessMakePrg
    --     : YiA startIdrisIDE
    --     : actions
    -- }
