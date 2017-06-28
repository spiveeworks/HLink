module Join (main) where

import Control.Monad

import System.Environment
import System.Directory

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
  children <- listDirectory root
  linkss <- forM children $ getLinksOrRecurse eval
  let links = concat linkss
  appendLinksIn comp links root

getLinksOrRecurse :: EvalMap -> FilePath -> IO [Link]
getLinksOrRecurse eval path = do
  isFile <- doesFileExist path
  if isFile
    then return []
    else do
      mDotLink <- getDotLinkIn path
      case mDotLink of
        Just dotLink -> getLinksFrom eval dotLink
        Nothing -> do
          children <- listDirectory path
          resultss <- forM children $ getLinksOrRecurse eval
          return $ concat resultss
  
  
