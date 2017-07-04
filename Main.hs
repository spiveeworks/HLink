module Main (main) where


import System.Environment

import Data.Char(toUpper)


import qualified Mark
import qualified Mount
import qualified Join

import qualified CompressPaths


main = getArgs >>= dispatch1

dispatch1 :: [String] -> IO ()
dispatch1 (command:args) = dispatch2 (map toUpper command) args
dispatch1 [] = dispatch2 "HELP" []

dispatch2 :: String -> [String] -> IO ()
dispatch2 "MARK"     = Mark.main
dispatch2 "MOUNT"    = Mount.main
dispatch2 "JOIN"     = Join.main
dispatch2 "COMPRESS" = CompressPaths.main
dispatch2 _          = \_ -> putStrLn "Use \"mark\", \"mount\", \"join\", or \"compress\" as first argument. "

