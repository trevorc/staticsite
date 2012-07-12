module Main where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.MIME.Types
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process
import Text.Printf
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M

import Codec.Compression.GZip


ensureDirectory :: FilePath -> IO ()
ensureDirectory path = do
  exists <- doesDirectoryExist path
  unless exists $ createDirectory path

needsUpdating :: FilePath -> FilePath -> IO Bool
needsUpdating x y = do
  missing <- not <$> doesFileExist y
  if missing
    then return True
    else x `isNewerThan` y
  where isNewerThan = liftA2 (>) `on` getModificationTime

gzip :: FilePath -> FilePath -> IO ()
gzip src dest =
    withFile src ReadMode $ \hSrc ->
        withFile dest WriteMode $ \hDest ->
            L.hGetContents hSrc >>=
                 (compress >>> L.hPut hDest)

isKnownExtension :: FilePath -> Bool
isKnownExtension = (`elem` [".html", ".js", ".css"])

main :: IO ()
main = do
  let buildDir = "build"
  bucket <- do
         args <- getArgs
         case args of
           []         -> takeFileName <$> getCurrentDirectory
           (bucket:_) -> return bucket
  ensureDirectory buildDir
  files <- map (id &&& (buildDir </>)) .
           filter (isKnownExtension . takeExtension) <$>
           getDirectoryContents "."
  filterM (uncurry needsUpdating) files >>= mapM_ (uncurry gzip)
  let groups = groupBy ((==)    `on` (takeExtension . fst)) .
               sortBy  (compare `on` (takeExtension . fst)) $
               files
  forM_ groups $ \fileGroup -> do
         let paths = map snd fileGroup
             extension = takeExtension (head paths)
             contentType = fromMaybe (error $ "unknown extension " ++
                                            extension) $
                                 M.lookup extension $
                                  typesMap defaultmtd
         rawSystem "s3cmd" $
                  [ "put"
                  , "-m", contentType
                  , "--add-header", "Content-Encoding: gzip"
                  , "--encoding", "UTF-8"
                  , "--acl-public"
                  ] ++ paths ++
                  [ printf "s3://%s/" bucket
                  ]
