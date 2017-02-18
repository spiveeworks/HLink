import System.Environment
import System.IO
import System.Directory

import Control.Monad

import Locations

main = do
  externs <- getArgs
  forM externs compressEach

compressEach :: FilePath -> IO ()
compressEach extern = do
  let extern' = extern ++ ".swp"
  uncompressed <- getLocationsF extern
  putLocations extern' uncompressed
  removeFile extern
  renameFile extern' extern

