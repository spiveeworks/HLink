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

mountEach :: EvalMap -> FilePath -> IO ()
mountEach eval path = do
  contents <- readFile (path </> "" <.> "links")
  let instructions = parse eval contents
  forM instructions createLink

parse :: EvalMap -> [String] -> [Link]
parse eval = map readLink . lines

createLink :: Link -> IO ()
createLink (Root path) = makeLink path "."
createLink (Relative source dest) = makeLink source dest
createLink (Switch name source switch) = do
  makeLink source dest
  
