module Mark (main) where

import System.Environment
import System.IO
import System.FilePath
import System.Directory

import Control.Monad

import PathCompression
import LinkIO

--mark folder(s) for linking
main :: IO ()
main = do
  (comp, eval) <- getPathMaps
  externs <- getArgs
  forM_ externs $ markEach comp

markEach :: CompMap -> FilePath -> IO ()
markEach comp path = do
  pathIsFile <- doesFileExist path
  let dirPath = if pathIsFile then takeDirectory path
                              else path
  let dotLinkPath = dirPath </> "" <.> "links"
  let linkLine = showLink . compressLink comp dirPath $ path :=>: path
  appendFile' dotLinkPath linkLine

appendFile' :: FilePath -> String -> IO ()
appendFile' file str = do
  exists <- doesFileExist file
  let join = if exists then "\n"
                       else ""
  appendFile file $ join ++ str

