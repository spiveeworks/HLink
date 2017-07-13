module Main
( mark
, mount
, join
, pull
, block

, compressPaths
) where

import System.Process
import System.IO (Handle)
import System.Environment (getArgs)

[mark, mount, join, compressPaths, pull, block] = map launch ["mark", "mount", "join", "compressPaths", "pull", "block"]

launch :: String -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
launch command = getArgs >>= createProcess . proc "hLink" . (command :)

