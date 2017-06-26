import System.Environment
import System.IO
import System.FilePath
import System.Directory

import Control.Monad

import PathCompression
import LinkIO (prettyDotLinkSeparator)

--mark folder(s) for linking
main = do
  (comp, eval) <- getPathMaps
  externs <- getArgs
  forM externs $ markEach comp

markEach :: CompMap -> FilePath -> IO ()
markEach comp path = do
  pathIsFile <- doesFileExist path
  appendFile (linkPath pathIsFile path) (linkLine pathIsFile comp path)

linkPath False = \ path -> path </> "" <.> "links"
linkPath True = linkPath False . takeDirectory

linkLine False = compressPath
linkLine True = \ comp path -> compressPath comp path ++ prettyDotLinkSeparator ++ takeFileName path

