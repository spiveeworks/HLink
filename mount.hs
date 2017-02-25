import System.Environment
import System.IO
import System.FilePath

import Control.Monad

import PathCompression
import LinkIO

main = do
  (comp, eval) <- getPathMaps
  args <- getArgs
  forM args $ mountEach eval

mountEach eval path = do
  contents <- readFile (path </> "" <.> "links")
  let instructions = parse eval contents
  forM instructions createLink

parse eval = map readLink . lines

createLink link = return ()
