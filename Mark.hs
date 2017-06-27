module Mark (main) where

import System.Environment
import System.IO
import System.FilePath
import System.Directory

import Control.Monad

import PathCompression
import LinkIO (prettyDotLinkSeparator)

--mark folder(s) for linking
main :: IO ()
main = do
  (comp, eval) <- getPathMaps
  externs <- getArgs
  forM_ externs $ markEach comp

markEach :: CompMap -> FilePath -> IO ()
markEach comp path = do
  pathIsFile <- doesFileExist path
  appendFile' (linkPath pathIsFile path) (linkLine pathIsFile comp path)

appendFile' :: FilePath -> String -> IO ()
appendFile' file str = do
  exists <- doesFileExist file
  let join = if exists then "\n"
                       else ""
  appendFile file $ join ++ str

linkPath :: Bool -> FilePath -> FilePath
linkPath False = \ path -> path </> "" <.> "links"
linkPath True = linkPath False . takeDirectory

linkLine :: Bool -> CompMap -> FilePath -> String
linkLine False = compressPath
linkLine True = \ comp path -> compressPath comp path ++ prettyDotLinkSeparator ++ takeFileName path

