module CompressPaths (main) where

import System.Environment
import System.IO
import System.Directory

import Control.Monad

import PathCompression

main = do
  externs <- getArgs
  forM externs compressEach

compressEach :: FilePath -> IO ()
compressEach extern = do
  let extern' = extern ++ ".swp"
  (comp, eval) <- getPathMapsF extern
  putCompMapF extern' comp
  removeFile extern
  renameFile extern' extern

