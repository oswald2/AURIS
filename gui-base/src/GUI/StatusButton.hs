module GUI.StatusButton
    ( buttonSetSendColor
    ) where

import           RIO

import           GI.Gtk

css :: ByteString
css =
    "button {\
    \   background-color: #ffff00;\
    \   color: black;\
    \}\
    \button:hover * {\
    \   background-color: #bbbb00;\
    \   color: black;\
    \}\
    \button:active {\
    \   background-color: #bbbb00;\
    \   color: black;\
    \}"


buttonSetSendColor :: Button -> IO ()
buttonSetSendColor bs = do
    provider <- cssProviderNew
    cssProviderLoadFromData provider css
    context <- widgetGetStyleContext bs
    styleContextAddProvider
        context
        provider
        (fromIntegral STYLE_PROVIDER_PRIORITY_APPLICATION)
    widgetSetName bs "send-button"
