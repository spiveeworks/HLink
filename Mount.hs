module Mount (main) where

import System.FilePath(takeDirectory, dropTrailingPathSeparator, (</>))
import System.Directory

import Control.Monad

import Data.Maybe (isJust)

import PathCompression
import LinkParsing (Link((:=>:)))
import LinkIO (getLinksNear)

main :: [String] -> IO ()
main args = do
  (comp, eval) <- getPathMaps
  forM_ args $ mountEach eval

mountEach :: EvalMap -> FilePath -> IO ()
mountEach eval path = do
  instructions <- getLinksNear eval path
  forM_ instructions $ attemptLink path


-- Creates a symbolic link as requested, unless something other than a symbolic link is in the way.
attemptLink :: FilePath -> Link -> IO ()
attemptLink path (source :=>: dest) = do
  is_used <- doesPathExist source'
  unless is_used $ createDirectoryIfMissing True $ takeDirectory $ dropTrailingPathSeparator source'
  is_available <- if is_used
                    then clearIfSymbolicAndPrint
                    else return True
  when is_available $ do
    createDirectoryLink dest source'
    putStrLn $ "Successfully created link at " ++ source
 where source' = path </> source
       clearIfSymbolicAndPrint = do
         m_old_dest <- clearIfSymbolic source'
         putStrLn $ case m_old_dest of
           Just old_dest -> "Removed old link: " ++ source ++ " -> " ++ old_dest
           Nothing -> "Failed to create link at: " ++ source
         return $ isJust m_old_dest


-- Removes symbolic links and reports their target, does not remove actual files/directories
clearIfSymbolic :: FilePath -> IO (Maybe FilePath)
clearIfSymbolic path = do
  result <- pathIsSymbolicLink path
  if result
    then do old_dest <- getSymbolicLinkTarget path
            removeLink path
            return (Just old_dest)
    else return Nothing

-- removes directory and file links portably
removeLink :: FilePath -> IO ()
removeLink path = do
  is_dir <- doesDirectoryExist path
  if is_dir
    then removeDirectoryLink path
    else removeFile path


