module Locations 
( LocalPath
, ExtrMap  -- maps location names to absolute paths
, CompMap  -- ExtrMap sorted by path length

, getLocations
, getLocationsF
, putLocations

, getLinkPath
, findLinkPath

, evaluateLocation

, compressLocation
, locationsBySize
) where

import Data.Map (Map, empty, insertWith, (!), toList)
import Data.Char (isSpace)
import Data.List (foldl', sortBy, stripPrefix)
import Data.Ord (comparing, Down(Down))
import Data.Maybe (fromJust)
import Data.Generics.Aliases (orElse)

import Control.Monad (liftM, mapM)

import System.FilePath
import System.Environment (lookupEnv)
import System.Directory (doesFileExist)

type LocalPath = String
type ExtrMap = Map String FilePath    -- used for evaluating local paths
type CompMap = [(String, FilePath)]   -- used for generating local paths
type NestMap = [(String, LocalPath)]  -- used for parsing/formatting above types


---- file IO + string parsing == file parsing

getLocations :: IO (ExtrMap)
getLocations = getLinkPath >>= getLocationsF

-- Read from a file then return contents parse by colon separated pairs
getNestedLocationsF :: FilePath -> IO NestMap
getNestedLocationsF = liftM parseLocations . readFile

-- performs getNestedLocations then convert terms to absolutepaths
getLocationsF :: FilePath -> IO (ExtrMap)
getLocationsF = liftM unnestLocations . getNestedLocationsF


-- recompress and store as if the output of getLocations
putLocations :: FilePath -> ExtrMap -> IO ()
putLocations hLinkPath = writeFile hLinkPath . compressAndFormat


-- of note: getLocationsF a >>= putLocations b
-- optimizes the compression of the set of names in file a, outputting to file b



---- getLinkPath :: IO FilePath

-- perform findLinkPath or fail without grace
getLinkPath :: IO FilePath
getLinkPath = liftM fromJust findLinkPath

-- Read from environment variable or search paths for .hLinks
findLinkPath :: IO (Maybe FilePath)
findLinkPath = do
  envPath <- lookupEnv "hLinkPath"
  foundPaths <- searchLinkPaths
  return $ foldr orElse Nothing $ envPath : foundPaths


-- search paths for .hLinks
searchLinkPaths :: IO [Maybe FilePath]
searchLinkPaths = getSearchPath >>= mapM checkLinkPath

-- check if .hLinks is at a path and if it is return the .hLinks path
checkLinkPath :: FilePath -> IO (Maybe FilePath)
checkLinkPath path = do
  isGoodPath <- doesFileExist hLinksPath
  if isGoodPath
     then return $ Just hLinksPath
     else return Nothing
  where hLinksPath = path </> "" <.> "hLink"



---- File -> Map

-- Pure part of getNestedLocations - parse string by colon separated pairs
parseLocations :: String -> NestMap
parseLocations = map parseEach . filter (elem ':') . lines
  where dropDelim = dropWhile isSpace . tail
        parseEach line = (location, dropDelim path)
          where (location, path) = break (== ':') line


-- pure part of getLocations -- convert locals to absolutes, using each conversion to convert the following lines
unnestLocations :: NestMap -> ExtrMap
unnestLocations = foldl' foldWith empty
  where foldWith paths (location, path) = insertWith (\new old -> old) location path' paths
          where path' = if hasDrive path 
                           then path 
                           else evaluateLocation paths path


-- inverse of compressLocation - turns a localpath into an absolute path
evaluateLocation :: ExtrMap -> LocalPath -> FilePath
evaluateLocation paths path = paths ! pathHead </> dropWhile isPathSeparator pathTail
  where (pathHead, pathTail) = break isPathSeparator path



---- Map -> File

-- pure part of putLocations
compressAndFormat :: ExtrMap -> String
compressAndFormat = unlines . reverse . map (\(name, path) -> name ++ ": " ++ path) . nestLocations . locationsBySize

-- translate from the output of locationsBySize into the output of unnestLocations
nestLocations :: CompMap -> NestMap
nestLocations paths = foldr compressAndAdd [] paths
  where compressAndAdd (locname, path) acc = (locname, compressLocation paths path) : acc

compressLocation :: CompMap -> FilePath -> LocalPath
-- you may want to make sure pathSeparators are consistent first
compressLocation paths path = foldr shortingFold path paths
  where shortingFold (basename, basepath) tryRest = case stripPrefixP basepath path of
          Nothing -> tryRest
          Just pathTail -> let pathTail' = dropWhile isPathSeparator pathTail
                           in if (not . null) pathTail'
                                 then basename </> pathTail'
                                 else tryRest

--  like stripPrefix but makes sure that a directory name doesn't get split in two
--  would be more efficient to reimplement from foldr I guess?
stripPrefixP :: FilePath -> FilePath -> Maybe FilePath
stripPrefixP basepath = stripPrefix basepath'
  where basepath' = init $ basepath </> "*"


-- sort the output of getLocations by length of path in steps; useful for undoing unnestLocations
locationsBySize :: ExtrMap -> CompMap 
-- descending by number of directories in path
locationsBySize = sortBy (comparing (Down . length . splitPath . snd)) . toList




