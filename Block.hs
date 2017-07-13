module Block (main) where

import Control.Monad

import System.FilePath
import System.IO

--mark folder(s) such that they don't get Join'd
main :: [String] -> IO ()
main = mapM_ blockEach


blockEach :: FilePath -> IO ()
blockEach path = appendFile (path </> "block" <.> "links") "\n"

