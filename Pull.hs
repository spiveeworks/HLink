module Pull(main) where

import Control.Monad

import System.Directory
import System.FilePath

import PathCompression
import LinkParsing(Link)
import LinkIO
import JoinBase(adjustConsumeLinksInSymLink)

main :: [String] -> IO ()
main roots = do
  (comp, eval) <- getPathMaps
  forM_ roots $ pullEach comp eval

pullEach :: CompMap -> EvalMap -> FilePath -> IO ()
pullEach comp eval oldRoot = do
  let newRoot = takeDirectory oldRoot
  isLink <- pathIsSymbolicLink oldRoot
  oldTarget <- if isLink then getSymbolicLinkTarget oldRoot
                         else return oldRoot
  links <- adjustConsumeLinksInSymLink eval newRoot oldRoot oldTarget
  appendLinksIn comp links newRoot


