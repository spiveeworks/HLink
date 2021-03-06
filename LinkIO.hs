module LinkIO
( getDotLinkIn

, getLinksNear
, getLinksFrom
, getLinksIn
, getLinksWith

, appendLinksNear
, appendLinksTo
, appendLinksIn
, appendLinksWith
) where

import System.IO
import System.Directory
import System.FilePath

import LinkParsing
import PathCompression (CompMap, EvalMap)


------ File Operations

---- File finding

appendDotLink :: FilePath -> FilePath
appendDotLink path = path </> "" <.> "links"

getDotLinkIn :: FilePath -> IO (Maybe FilePath)
getDotLinkIn path = do
  let dotLink = appendDotLink path
  dotLinkExists <- doesFileExist dotLink
  return $ if dotLinkExists then Just dotLink
                            else Nothing

getNearestDir :: FilePath -> IO FilePath
getNearestDir path = do
  isFile <- doesFileExist path
  return $ if isFile then takeDirectory path
                     else path
  

---- File Reading

-- Takes a directory or file path and parses an adjacent .links if any
getLinksNear :: EvalMap -> FilePath -> IO [Link]
getLinksNear eval path = getNearestDir path >>= getLinksIn eval

-- Takes a .links file and parses it
getLinksFrom :: EvalMap -> FilePath -> IO [Link]
getLinksFrom eval filePath = getLinksWith eval dirPath filePath
  where dirPath = takeDirectory filePath

-- Takes a directory and parses a contained .links if any
getLinksIn :: EvalMap -> FilePath -> IO [Link]
getLinksIn eval dirPath = do
  mFilePath <- getDotLinkIn dirPath
  case mFilePath of
    Nothing -> return []
    Just filePath -> getLinksWith eval dirPath filePath

-- Like parseLinks but with a FilePath instead of a String
getLinksWith :: EvalMap -> FilePath -> FilePath -> IO [Link]
getLinksWith eval dirPath filePath = do  -- note the order
  str <- readFile filePath
  return $ parseLinks eval dirPath str


---- File Writing

-- Takes a directory or file path and appends some links to its .links file
appendLinksNear :: CompMap -> [Link] -> FilePath -> IO ()
appendLinksNear comp links path = getNearestDir path >>= appendLinksIn comp links

-- Takes a .links file and adds some links to it
appendLinksTo :: CompMap -> [Link] -> FilePath -> IO ()
appendLinksTo comp links filePath = appendLinksWith comp links dirPath filePath
  where dirPath = takeDirectory filePath

-- Takes a directory path and does the same
appendLinksIn :: CompMap -> [Link] -> FilePath -> IO ()
appendLinksIn comp links dirPath = appendLinksWith comp links dirPath filePath
  where filePath = appendDotLink dirPath

-- like formatLinks but appends as lines to a file
appendLinksWith :: CompMap -> [Link] -> FilePath -> FilePath -> IO ()
appendLinksWith comp links dirPath filePath = do
  exists <- doesFileExist filePath
  lastChar <- if exists then getLastChar filePath
                        else return '\n'  -- mainly so that it doesn't throw
  let join = if lastChar /= '\n' then "\n"
                                 else ""
  appendFile filePath $ join ++ str
  where str = formatLinks comp dirPath links


getLastChar :: FilePath -> IO Char
getLastChar path = withFile path ReadMode getLastChar'

getLastChar' :: Handle -> IO Char
getLastChar' hFile = do
  hSeek hFile SeekFromEnd (-1)
  hGetChar hFile
