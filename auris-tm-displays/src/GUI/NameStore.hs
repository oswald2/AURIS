module GUI.NameStore
    ( NameStore
    , nameStoreIterToIndex
    , nameStoreNew
    , nameStoreGetValue
    , nameStoreGetValueIdx
    , nameStoreWriteValue
    , nameStoreUpdateValue
    , nameStoreAppendValue
    ) where


import           RIO
import qualified RIO.Text                      as T

import           Data.GI.Base                   ( new )
import           Data.GI.Base.BasicTypes        ( GObject
                                                , ManagedPtr(..)
                                                , TypedObject(..)
                                                )
-- import           Data.GI.Base.ManagedPtr        ( withManagedPtr )
import           Data.GI.Base.Overloading       ( HasParentTypes
                                                , ParentTypes
                                                )
import           Data.GI.Gtk.ModelView.CustomStore
                                                -- ( CustomStore(..)
                                                -- , DragDestIface(..)
                                                -- , DragSourceIface(..)
                                                -- , TreeModelIface(..)
                                                -- , customStoreGetPrivate
                                                -- , customStoreGetStamp
                                                -- , customStoreNew
                                                -- )
import           Data.GI.Gtk.ModelView.Types
import           Foreign.Ptr                    ( nullPtr )
-- import           GI.GObject.Objects.Object      ( Object(..) )
-- import           GI.Gtk.Flags                   ( TreeModelFlags(..) )
-- import           GI.Gtk.Functions               ( treeGetRowDragData
--                                                 , treeSetRowDragData
--                                                 )
import           GI.Gtk.Interfaces.TreeModel
import           GI.Gtk.Structs.TreeIter        ( TreeIter(..)
                                                , getTreeIterUserData
                                                , setTreeIterStamp
                                                , setTreeIterUserData
                                                , setTreeIterUserData2
                                                , setTreeIterUserData3
                                                )
import           Unsafe.Coerce                  ( unsafeCoerce )

import           Data.HashTable.IO             as HT
import           Data.Vector                   as V
                                                ( freeze, toList)
import           Data.Vector.Mutable           as V

import           General.Types                  ( HasName(..) )

newtype NameStore a = NameStore (ManagedPtr (CustomStore (Store a) a))

data Store a = Store
    { storeVector    :: !(IOVector (Maybe a))
    , storeFill      :: !(IORef Int)
    , storeHashTable :: !(BasicHashTable Text Int)
    }

newStore :: (HasName a, MonadIO m) => [a] -> Int -> m (Store a)
newStore ls maxSize = liftIO $ do
    let size = max (RIO.length ls) maxSize
    ref <- newIORef 0
    -- create the new vector with the max size 
    vec <- V.new size
    V.set vec Nothing
    -- fill the vector 
    fill vec ls 0

    ht <- HT.new

    let update idx (Just val) = do
            let name = getName val
            HT.insert ht name idx
            writeIORef ref (idx + 1)
        update _idx Nothing = return ()


    iforM_ vec update
    return $ Store vec ref ht

  where
    fill _   []       _  = return ()
    fill vec (x : xs) !i = do
        V.unsafeWrite vec i (Just x)
        fill vec xs (i + 1)


mkNameStore :: CustomStore (Store a) a -> NameStore a
mkNameStore (CustomStore ht) = NameStore ht


instance HasParentTypes (NameStore a)
type instance ParentTypes (NameStore a) = '[TreeModel]

instance TypedObject (NameStore a) where
    glibType = glibType @TreeModel

instance GObject (NameStore a)

instance IsTypedTreeModel NameStore

nameStoreNew :: (HasName a, MonadIO m) => [a] -> Int -> m (NameStore a)
nameStoreNew ls maxSize = do
    store <- newStore ls maxSize

    let
        modelIface = TreeModelIface
            { treeModelIfaceGetFlags      = return [TreeModelFlagsListOnly]
            , treeModelIfaceGetIter       = \path -> do
                [n] <- treePathGetIndices' path
                len <- readIORef (storeFill store)
                if inRange (0, len) (fromIntegral n)
                    then Just <$> nameStoreIterNew 0 n
                    else return Nothing
            , treeModelIfaceGetPath       = \iter -> do
                                                n <- nameStoreIterToIndex iter
                                                treePathNewFromIndices' [n]
            , treeModelIfaceGetRow        = \iter -> do
                n   <- nameStoreIterToIndex iter
                len <- readIORef (storeFill store)
                if inRange (0, len) (fromIntegral n)
                    then do
                        val <- V.unsafeRead (storeVector store) (fromIntegral n)
                        case val of
                            Just v -> return v
                            Nothing ->
                                fail
                                    "NameStore.getRow: iter does not refer to a valid entry (Nothing)"
                    else
                        fail
                            "NameStore.getRow: iter does not refer to a valid entry"
            , treeModelIfaceIterNext      = \iter -> do
                n   <- nameStoreIterToIndex iter
                len <- readIORef (storeFill store)
                let newIter = n + 1
                if inRange (0, len) (fromIntegral newIter)
                    then Just <$> nameStoreIterNew 0 newIter
                    else return Nothing
            , treeModelIfaceIterChildren  = \index -> do
                case index of
                    Nothing | not (V.null (storeVector store)) ->
                        Just <$> nameStoreIterNew 0 0
                    _ -> return Nothing
            , treeModelIfaceIterHasChild  = \_ -> return False
            , treeModelIfaceIterNChildren = \index -> do
                case index of
                    Nothing -> readIORef (storeFill store)
                    _       -> return 0
            , treeModelIfaceIterNthChild  = \index n -> do
                case index of
                    Nothing -> Just <$> nameStoreIterNew 0 (fromIntegral n)
                    _       -> return Nothing
            , treeModelIfaceIterParent    = \_ -> return Nothing
            , treeModelIfaceRefNode       = \_ -> return ()
            , treeModelIfaceUnrefNode     = \_ -> return ()
            }

    customStoreNew store mkNameStore modelIface Nothing Nothing


inRange :: Ord a => (a, a) -> a -> Bool
inRange (l, h) n = n >= l && n < h


nameStoreIterNew :: MonadIO m => Int32 -> Int32 -> m TreeIter
nameStoreIterNew stamp u = do
    i <- Data.GI.Base.new TreeIter []
    setTreeIterStamp i stamp
    setTreeIterUserData i $ (unsafeCoerce u)
    setTreeIterUserData2 i nullPtr
    setTreeIterUserData3 i nullPtr
    return i

nameStoreIterToIndex :: MonadIO m => TreeIter -> m Int32
nameStoreIterToIndex i = unsafeCoerce <$> getTreeIterUserData i


nameStoreGetValue :: MonadIO m => NameStore a -> Text -> m (Maybe a)
nameStoreGetValue (NameStore st) name = liftIO $ do
    let store = customStoreGetPrivate (CustomStore st)
    idx <- HT.lookup (storeHashTable store) name
    case idx of
        Just i  -> V.unsafeRead (storeVector store) i
        Nothing -> return Nothing

nameStoreGetValueIdx :: MonadIO m => NameStore a -> Int -> m (Maybe a)
nameStoreGetValueIdx (NameStore st) idx = liftIO $ do
    let store = customStoreGetPrivate (CustomStore st)
    if idx >= 0 && idx < V.length (storeVector store)
        then V.unsafeRead (storeVector store) idx
        else return Nothing

nameStoreWriteValue :: MonadIO m => NameStore a -> Text -> a -> m ()
nameStoreWriteValue (NameStore st) name val = liftIO $ do
    let store = customStoreGetPrivate (CustomStore st)
    idx <- HT.lookup (storeHashTable store) name
    case idx of
        Just i -> do
            V.unsafeWrite (storeVector store) i (Just val)
            rowChanged st i
        Nothing -> return ()

nameStoreUpdateValue
    :: MonadIO m
    => NameStore a
    -> Text
    -> (Maybe a -> a -> Maybe a)
    -> a
    -> m ()
nameStoreUpdateValue (NameStore st) name cmp val = liftIO $ do
    let store = customStoreGetPrivate (CustomStore st)
    idx <- HT.lookup (storeHashTable store) name
    case idx of
        Just i -> do
            oldVal <- V.unsafeRead (storeVector store) i
            V.unsafeWrite (storeVector store) i (cmp oldVal val)
            rowChanged st i
        Nothing -> return ()


nameStoreAppendValue :: (HasName a, MonadIO m) => NameStore a -> a -> m Bool
nameStoreAppendValue (NameStore st) val = liftIO $ do
    let store = customStoreGetPrivate (CustomStore st)
    n <- readIORef (storeFill store)
    let !newIdx  = n
        !newSize = newIdx + 1
    if newIdx < V.length (storeVector store)
        then do
            -- traceM $ "nameStoreAppendValue newIdx=" <> T.pack (show newIdx)
            V.unsafeWrite (storeVector store) newIdx (Just val)
            HT.insert (storeHashTable store) (getName val) newIdx
            writeIORef (storeFill store) newSize
            rowInserted st newIdx
            return True
        else return False

rowChanged :: ManagedPtr (CustomStore private row) -> Int -> IO ()
rowChanged st i = do
    stamp <- customStoreGetStamp (CustomStore st)
    -- traceM $ "rowChanged: stamp=" <> T.pack (show stamp)
    path <- treePathNewFromIndices' [fromIntegral i]
    iter <- nameStoreIterNew stamp (fromIntegral i)
    treeModelRowChanged (CustomStore st) path iter

rowInserted :: ManagedPtr (CustomStore private row) -> Int -> IO ()
rowInserted st i = do
    stamp <- customStoreGetStamp (CustomStore st)
    -- traceM $ "rowInserted: stamp=" <> T.pack (show stamp)
    path <- treePathNewFromIndices' [fromIntegral i]
    iter <- nameStoreIterNew stamp (fromIntegral i)
    treeModelRowInserted (CustomStore st) path iter
