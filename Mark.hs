module Mark (main) where

import Control.Monad

import PathCompression
import LinkParsing (Link((:=>:)))
import LinkIO (appendLinksNear)

--mark folder(s) for linking
main :: [String] -> IO ()
main externs = do
  (comp, eval) <- getPathMaps
  forM_ externs $ markEach comp

markEach :: CompMap -> FilePath -> IO ()
markEach comp path = appendLinksNear comp [path :=>: path] path

