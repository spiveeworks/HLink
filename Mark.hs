module Mark (main) where

import System.Environment
import System.IO
import System.FilePath
import System.Directory

import Control.Monad

import PathCompression
import LinkParsing (Link((:=>:)))
import LinkIO (appendLinksNear)

--mark folder(s) for linking
main :: IO ()
main = do
  (comp, eval) <- getPathMaps
  externs <- getArgs
  forM_ externs $ markEach comp

markEach :: CompMap -> FilePath -> IO ()
markEach comp path = appendLinksNear comp [path :=>: path] path

