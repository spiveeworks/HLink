module PathCompression
( FilePath'
, EvalMap  -- maps location names to absolute paths
, CompMap  -- EvalMap sorted by path length

, getPathMaps  -- getLinkPath >>= getPathMapsF
, getPathMapsF
, putCompMapF
  -- use putCompMapF on a temporary/swap file

, getLinkPath  -- liftM fromJust findLinkPath
, findLinkPath

, evaluatePath
, compressPath
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
import System.Directory (findFile)


type FilePath' = String

type EvalMap = Map String FilePath    -- used for evaluating local paths
type CompMap = [(String, FilePath)]   -- used for generating local paths
type NestMap = [(String, FilePath')]  -- used for parsing/formatting above types


hLinkSeparator :: Char
hLinkSeparator = ':'

refHead :: Char
refHead = '$'

($:) :: FilePath -> FilePath'
($:) path = refHead : path

------ impure (file operations)

---- getPathMaps

-- parseFile = readFile + parseString
getPathMaps :: IO (CompMap, EvalMap)
getPathMaps = getLinkPath >>= getPathMapsF


-- performs getNestedLocations then convert terms to absolutepaths
getPathMapsF :: FilePath -> IO (CompMap, EvalMap)
getPathMapsF = liftM convert . getNestMapF
  where convert nest = (comp, eval)
          where comp = toComp eval
                eval = toEval nest


-- Read from a file then return contents parse by colon separated pairs
getNestMapF :: FilePath -> IO NestMap
getNestMapF = liftM readNest . readFile



---- putCompMapF

-- gets a CompMap and reformats it ready to be read by getLocationsF
putCompMapF :: FilePath -> CompMap -> IO ()
putCompMapF hLinkPath = writeFile hLinkPath . showNest . toNest



---- getLinkPath

-- perform findLinkPath or fail without grace
getLinkPath :: IO FilePath
getLinkPath = liftM fromJust findLinkPath


-- Read from environment variable or search paths for .hLinks
findLinkPath :: IO (Maybe FilePath)
findLinkPath = do
  envPath <- lookupEnv "hLinkPath"
  case envPath of
    Just path -> return envPath
    Nothing   -> searchLinkPaths

-- search paths for .hLinks
searchLinkPaths :: IO (Maybe FilePath)
searchLinkPaths = do
  paths <- getSearchPath
  findFile paths ("" <.> "hLink")




------ PathMap manipulation and usage

---- String <-> NestMap

readNest :: String -> NestMap
readNest = map parseEach . filter (elem hLinkSeparator) . lines
  where dropDelim = dropWhile isSpace . tail
        parseEach line = (location, dropDelim path)
          where (location, path) = break (== hLinkSeparator) line


showNest :: NestMap -> String
showNest = unlines . reverse . map showEach
  where showEach (name, path) = name ++ hLinkSeparator : " " ++ path



---- PathMap conversion cycle

toEval :: NestMap -> EvalMap
toEval = foldl' foldWith empty
  where foldWith paths (location, path) = insertWith (flip const) location path' paths
          where path' = evaluatePath paths path


toComp :: EvalMap -> CompMap
toComp = sortBy (comparing (Down . length . splitPath . snd)) . toList


toNest :: CompMap -> NestMap
toNest paths = foldr compressAndAdd [] paths
  where compressAndAdd (locname, path) acc = (locname, compressPath paths path) : acc



---- PathMap usage - FilePath compression

-- you may want to make sure pathSeparators are consistent when using this
compressPath :: CompMap -> FilePath -> FilePath'
compressPath paths path = foldr shortingFold path paths
  where shortingFold (basename, basepath) tryRest = case stripPrefixP basepath path of
          Nothing -> tryRest
          Just pathTail -> let pathTail' = dropWhile isPathSeparator pathTail
                           in if (not . null) pathTail'
                                 then ($:) basename </> pathTail'
                                 else tryRest
        stripPrefixP = stripPrefix . addTrailingPathSeparator


evaluatePath :: EvalMap -> FilePath' -> FilePath
evaluatePath paths (c : path)
  | c == refHead = paths ! pathHead </> dropWhile isPathSeparator pathTail
  where (pathHead, pathTail) = break isPathSeparator path
evaluatePath _ path = path


