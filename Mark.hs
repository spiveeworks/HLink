import System.Environment
import System.IO
import System.FilePath

import Control.Monad

import Locations

--mark folder(s) for linking
main = do
  unpaths <- liftM locationsBySize getLocations
  externs <- getArgs
  forM externs $ markEach unpaths

markEach :: CompMap -> FilePath -> IO ()
markEach unpaths extern = appendFile (extern </> "" <.> "links") . compressLocation unpaths $ (extern ++ "\n")

