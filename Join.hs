module Join (main) where

import Control.Monad

import System.Environment
import System.Directory (doesFileExist, listDirectory, removeFile)
import System.FilePath (normalise, (</>))

import PathCompression
import LinkParsing
import LinkIO

main :: IO ()
main = do
  (comp, eval) <- getPathMaps
  roots <- getArgs
  forM_ roots $ joinEach comp eval

joinEach :: CompMap -> EvalMap -> FilePath -> IO ()
joinEach comp eval root = do
  links <- catCallContents root $ getLinksOrRecurse eval
  appendLinksIn comp links root

getLinksOrRecurse :: EvalMap -> FilePath -> IO [Link]
getLinksOrRecurse eval path = do
  isFile <- doesFileExist path
  if isFile
    then return []
    else do
      mDotLink <- getDotLinkIn path
      case mDotLink of
        Just dotLink -> do
          links <- getLinksFrom eval dotLink
          length links `seq` removeFile dotLink
          return links
        Nothing -> catCallContents path $ getLinksOrRecurse eval

catCallContents :: FilePath -> (FilePath -> IO [a]) -> IO [a]
catCallContents path action = do
  contents <- listDirectory path
  let absContents = map (\ name -> normalise $ path </> name) contents
  linkss <- forM absContents action  -- in theory these don't have to be links
  return $ concat linkss
