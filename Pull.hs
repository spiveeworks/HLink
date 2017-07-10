module Join(main) where

import Control.Monad

import System.Directory
import System.FilePath

import PathCompression
import LinkParsing(Link)
import LinkIO

main :: [String] -> IO ()
main roots = do
  (comp, eval) <- getPathMaps
  forM_ roots $ pullEach comp eval

pullEach :: CompMap -> EvalMap -> FilePath -> IO ()
pullEach comp eval oldRoot = do
  let newRoot = takeDirectory oldRoot
  isLink <- pathIsSymbolicLink oldRoot
  oldTarget <- if isLink then getLinkTarget oldRoot
                         else return oldRoot
  links <- adjustConsumeLinks eval newRoot oldRoot oldTarget
  appendLinksIn comp links newRoot


