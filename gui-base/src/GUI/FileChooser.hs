module GUI.FileChooser
  ( addFilterPatterns
  ) 
where


import           RIO
import           GI.Gtk                        as Gtk



addFilterPatterns :: (IsFileChooser a) => a -> [(Text,Text)] -> IO ()
addFilterPatterns fc patterns = do
    mapM_ newFilt patterns
  where
    newFilt (name, pat) = do
        f <- fileFilterNew
        fileFilterAddPattern f pat
        fileFilterSetName f (Just name)
        fileChooserAddFilter fc f
