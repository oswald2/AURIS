module GUI.StatusEntry
  (EntryStatus(..)
  , StatusEntry
  , statusEntrySetState
  , statusEntrySetupCSS
  )
where


import           RIO

import           GI.Gtk                        as Gtk



newtype StatusEntry = StatusEntry Entry 

css :: ByteString
css =
    "#error-entry {\
      \   background-color: #ff0000;\
      \   color: white;\
      \}\
      \#warn-entry {\
      \   background-color: #ffff00;\
      \   color: black;\
      \}\
      \#green-entry {\
      \   background-color: #00BB00;\
      \   color: black;\
      \}"

data EntryStatus = 
  ESGreen 
  | ESWarn
  | ESError 
  deriving (Enum, Ord, Eq, Show)

statusEntrySetState :: StatusEntry -> EntryStatus -> Text -> IO ()
statusEntrySetState (StatusEntry e) st text = do
  case st of
      ESError -> do
        entrySetText e text
        widgetSetName e "error-entry"
      ESWarn -> do 
        entrySetText e text
        widgetSetName e "warn-entry"
      ESGreen  -> do
        entrySetText e text 
        widgetSetName e "green-entry"


statusEntrySetupCSS :: Entry -> IO StatusEntry
statusEntrySetupCSS entry = do
    widgetSetSensitive entry False
    provider <- cssProviderNew
    cssProviderLoadFromData provider css
    context <- widgetGetStyleContext entry
    styleContextAddProvider context provider 600
    return (StatusEntry entry)

