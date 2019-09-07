module General.ShowConduit 
(
    showConduit
)
where



import Conduit 


showConduit :: (Show a, MonadIO m) => ConduitT a a m ()
showConduit = awaitForever $ \x -> do
    liftIO $ print x
    yield x