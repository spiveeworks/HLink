module Locations 
( LocalPath
, getLocations
, evaluateLocation
, locationsBySize
, compressLocation
, putLocations
) where

import Data.Map (Map, empty, insertWith, (!), toList)
import Data.Char (isSpace)
import Data.List (foldl', sortBy, stripPrefix)
import Data.Ord (comparing, Down(Down))

import System.FilePath

type LocalPath = String


-- Read from a file then return contents parse by colon separated pairs
getNestedLocations :: FilePath -> IO [(String, LocalPath)]
getNestedLocations hLinkPath = do
  locations <- readFile hLinkPath
  return (parseLocations locations)

-- Pure part of getNestedLocations - parse string by colon separated pairs
parseLocations :: String -> [(String, LocalPath)]
parseLocations = map parseEach . filter (elem ':') . lines
  where dropDelim = dropWhile isSpace . tail
        parseEach line = (location, dropDelim path)
          where (location, path) = break (== ':') line


-- inverse of compressLocation - turns a localpath into an absolute path
evaluateLocation :: Map String FilePath -> LocalPath -> FilePath
evaluateLocation paths path = paths ! pathHead </> dropWhile isPathSeparator pathTail
  where (pathHead, pathTail) = break isPathSeparator path


-- performs getNestedLocations then convert terms to absolutepaths
getLocations :: FilePath -> IO (Map String FilePath)
getLocations hLinkPath = do
  nested <- getNestedLocations hLinkPath
  return (unnestLocations nested)

-- pure part of getLocations -- convert locals to absolutes, using each conversion to convert the following lines
unnestLocations :: [(String, LocalPath)] -> Map String FilePath
unnestLocations = foldl' foldWith empty
  where foldWith paths (location, path) = insertWith (\new old -> old) location path' paths
          where path' = if hasDrive path 
                           then path 
                           else evaluateLocation paths path


-- sort the output of getLocations by length of path in steps; useful for undoing unnestLocations
locationsBySize :: Map String FilePath -> [(String, FilePath)] 
-- descending by number of directories in path
locationsBySize = sortBy (comparing (Down . length . splitPath . snd)) . toList

-- translate from the output of locationsBySize into the output of unnestLocations
nestLocations :: [(String, FilePath)] -> [(String, LocalPath)]
nestLocations paths = foldr compressAndAdd [] paths
  where compressAndAdd (locname, path) acc = (locname, compressLocation paths path) : acc


compressLocation :: [(String, FilePath)] -> FilePath -> LocalPath
-- you may want to make sure pathSeparators are consistent first
compressLocation paths path = foldr shortingFold path paths
  where shortingFold (basename, basepath) tryRest = case stripPrefix basepath path of
          Nothing -> tryRest
          Just pathTail -> let pathTail' = dropWhile isPathSeparator pathTail
                           in if (not . null) pathTail'
                                 then basename </> pathTail'
                                 else tryRest


-- recompress and store the output of getLocations
putLocations :: FilePath -> Map String FilePath -> IO ()
putLocations hLinkPath = writeFile hLinkPath . compressAndFormat

-- pure part of putLocations
compressAndFormat :: Map String FilePath -> String
compressAndFormat = unlines . reverse . map (\(name, path) -> name ++ ": " ++ path) . nestLocations . locationsBySize

-- of note: getLocations a >>= putLocations b
-- optimizes the compression of the set of names in file a, outputting to file b

