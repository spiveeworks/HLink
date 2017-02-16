import System.Environment
import System.IO
import System.FilePath

import Control.Monad

--mark folder(s) for linking
main = do
  externs <- getArgs
  forM externs markEach

markEach :: FilePath -> IO ()
markEach extern = appendFile (extern </> "" <.> "links") (extern ++ "\n")


