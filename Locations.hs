module Locations 
( LocalPath
, getLocations
, evaluateLocation
) where

import Data.Map (Map, empty, insertWith, (!))
import Data.Char (isSpace)
import Data.List (foldl')

import System.FilePath

type LocalPath = String


getNestedLocations :: FilePath -> IO [(String, LocalPath)]
getNestedLocations hLinkPath = do
  locations <- readFile hLinkPath
  return (parseLocations locations)

parseLocations :: String -> [(String, LocalPath)]
parseLocations = map parseEach . lines
  where dropDelim = dropWhile isSpace . tail
        parseEach line = (location, dropDelim path)
          where (location, path) = break (== ':') line


evaluateLocation :: Map String FilePath -> LocalPath -> FilePath
evaluateLocation paths path = paths ! pathHead </> dropWhile isPathSeparator pathTail
  where (pathHead, pathTail) = break isPathSeparator path


getLocations :: FilePath -> IO (Map String FilePath)
getLocations hLinkPath = do
  nested <- getNestedLocations hLinkPath
  return (unnestLocations nested)

unnestLocations :: [(String, LocalPath)] -> Map String FilePath
unnestLocations = foldl' foldWith empty
  where foldWith paths (location, path) = insertWith (\new old -> old) location path' paths
          where path' = if hasDrive path 
                           then path 
                           else evaluateLocation paths path

