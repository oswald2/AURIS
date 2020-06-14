module Data.MIB.Load
  ( loadFromFileGen
  , GenericParse(..)
  )
where

import           RIO

import           RIO.Char
import           RIO.FilePath
import           RIO.Directory
import qualified RIO.ByteString.Lazy           as B
import qualified RIO.Text                      as T

import qualified RIO.Vector                    as V
import qualified Data.ByteString.Lazy.Char8    as BC
import           Data.Csv


-- | This class provides @genericParse@ function that generates record parser
-- from data constructor.
-- It uses type-class magic to recursively parse each of the constructor's
-- arguments from the input vector.
class GenericParse ctr res where

  -- | Accepts predicate on the input vector length.
  genericParse :: (Int -> Bool) -> ctr -> Record -> Parser res
  genericParse validLen ctr rec
    | validLen $ V.length rec = genericParse' 0 ctr rec
    | otherwise = mzero

  -- | This is the function that loops.
  genericParse' :: Int -> ctr -> Record -> Parser res


instance GenericParse res res where
  genericParse' _ res _ = pure res


instance (FromField arg, GenericParse ctr res)
  => GenericParse (arg -> ctr) res where
  genericParse' ix ctr rec = do
    ctr' <- ctr <$> index rec ix
    genericParse' (ix+1) ctr' rec



myOptions :: DecodeOptions
myOptions = defaultDecodeOptions { decDelimiter = fromIntegral (ord '\t') }


loadFromFileGen
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack, FromRecord a)
  => FilePath
  -> FilePath
  -> m (Either Text (Vector a))
loadFromFileGen mibPath fileName = do
  let file = mibPath </> fileName
  ex <- doesFileExist file
  if ex
    then do
      logInfo $ "Reading file " <> display (T.pack fileName)
      content <- B.readFile file
      logInfo "File read. Parsing..."
      let !r = decodeWith myOptions NoHeader (BC.filter isAscii content)
      logInfo "Parsing Done."
      case r of
        Left  err -> pure $ Left (T.pack err)
        Right x   -> pure (Right x)
    else do
      return $! Left $ "File " <> T.pack file <> " does not exist."
