-- |
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

module JReduce.Target where

-- lens
import           Control.Lens

-- -- deepseq
-- import           Control.DeepSeq

-- zip-archive
import           Codec.Archive.Zip

-- bytestring
import qualified Data.ByteString.Lazy                  as BL

-- dirtree
import           System.DirTree
import           System.DirTree.Zip

-- nfdata
import Control.DeepSeq

-- base
import           Control.Exception
import           GHC.Generics (Generic)
import           Data.Maybe

-- jvhms
import           Jvmhs


-- | The content of a file is either a Class, a Jar, or some MetaData.
data Content
  = ClassFile Class
  | Jar (DirForest Content)
  | MetaData !BL.ByteString
  deriving (Show, Eq, Generic)

makePrisms ''Content

instance NFData Content

-- | We define the target of the reduction as
type Target = DirTree Content

deepenTarget :: DirTree BL.ByteString -> IO Target
deepenTarget = imapM (readTargetFile . fileKeyToPath) where
  readTargetFile :: FilePath -> BL.ByteString -> IO Content
  readTargetFile fp bst =
    case asClassName fp of
      Just _ ->
        return $ ClassFile (either (error . show) fromClassFile $ readClassFile' True bst)
      Nothing
        | isJar fp -> handle (\(SomeException _) -> return $ MetaData bst) $ do
            d1 <- maybe (fail "Jar contains external links") return
              <$> followInternalLinks . directory $ toArchive bst ^. files
            d2 <- imapM (readTargetFile . fileKeyToPath) d1
            return . Jar $ d2 ^?! _Directory
        | otherwise -> return $ MetaData bst

undeepenTarget :: Target -> DirTree BL.ByteString
undeepenTarget = fmap writeTargetFile where
    writeTargetFile :: Content -> BL.ByteString
    writeTargetFile = \case
      ClassFile cl -> serializeClass cl
      MetaData b -> b
      Jar f ->
        fromArchive
        . (files .~ (Real . writeTargetFile <$> f))
        $ emptyArchive

followInternalLinks :: RelativeDirTree Link a -> Maybe (DirTree a)
followInternalLinks tree =
  let
    x = flip flattenDirTree tree $ \case
      File (Real a) -> Just (file a)
      File (Symlink (External _)) -> Nothing
      File (Symlink (Internal f)) -> x ^? _Just . ix f
      Directory fm ->
        Just
        . directory' . mapMaybe (\(i, m) -> (i,) <$> m) . itoList
        $ fm
  in x


displayTarget :: Target -> IO ()
displayTarget = go "" where
  go prefix =
    ifoldMap $ \fk a -> do
      putStr prefix >> putStr (show fk) >> putStr "  ->  "
      case a of
        MetaData _ -> putStrLn "[data]"
        Jar t -> putStrLn "[jar]" >> go (prefix ++ "> ") (directory t)
        ClassFile cls -> print (cls ^. className)
