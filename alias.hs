module Main
( mark
, mount
, join

, compressPaths
) where

import System.Process
import System.IO (Handle)
import System.Environment (getArgs)

[mark, mount, join, compressPaths] = map launch ["mark", "mount", "join", "compressPaths"]

launch :: String -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
launch command = getArgs >>= createProcess . proc "hLink" . (command :)

