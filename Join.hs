module Join (main) where

import Control.Monad

import System.Directory
import System.FilePath

import PathCompression
import LinkParsing(Link)
import LinkIO

main :: [String] -> IO ()
main roots = do
  (comp, eval) <- getPathMaps
  forM_ roots $ joinEach comp eval

joinEach :: CompMap -> EvalMap -> FilePath -> IO ()
joinEach comp eval root = do
  links <- catCallContents root $ getLinksOrRecurse eval root
  appendLinksIn comp links root

getLinksOrRecurse :: EvalMap -> FilePath -> FilePath -> IO [Link]
getLinksOrRecurse eval root path = do
  isFile <- doesFileExist path       -- there is nothing we can do with a file
  isLink <- pathIsSymbolicLink path  -- on one hand we don't want loops, 
                                     -- on the other the target could be intended as separate
  if isFile || isLink
    then return []
    else do
      mDotLink <- getDotLinkIn path
      case mDotLink of
        Just dotLink -> do
          links <- getLinksFrom eval dotLink
          length links `seq` removeFile dotLink
          return $ map adjust links
        Nothing -> catCallContents path $ getLinksOrRecurse eval root
  where adjust link@(source :=>: dest)
          | isRelative source = (normalise $ path' </> source) :=>: dest
          | otherwise = link
        path' = makeRelative root path


catCallContents :: FilePath -> (FilePath -> IO [a]) -> IO [a]
catCallContents path action = do
  contents <- listDirectory path
  let absContents = map (\ name -> normalise $ path </> name) contents
  linkss <- forM absContents action  -- in theory these don't have to be links
  return $ concat linkss
