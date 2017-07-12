module CompressPaths (main) where

import System.Directory

import Control.Monad

import PathCompression

main :: [String] -> IO ()
main = mapM_ compressEach

compressEach :: FilePath -> IO ()
compressEach extern = do
  let extern' = extern ++ ".swp"
  (comp, eval) <- getPathMapsF extern
  putCompMapF extern' comp
  removeFile extern
  renameFile extern' extern

