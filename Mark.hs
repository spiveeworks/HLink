import System.Environment
import System.IO
import System.FilePath

import Control.Monad

import PathCompression

--mark folder(s) for linking
main = do
  (comp, eval) <- getPathMaps
  externs <- getArgs
  forM externs $ markEach comp

markEach :: CompMap -> FilePath -> IO ()
markEach comp extern = appendFile (extern </> "" <.> "links") . compressPath comp $ (extern ++ "\n")

