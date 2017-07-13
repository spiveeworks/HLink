module Join(main) where

import Control.Monad

import System.Directory
import System.FilePath

import JoinBase (adjustConsumeLinksInFile)
import PathCompression
import LinkParsing(Link)
import LinkIO

main :: [String] -> IO ()
main roots = do
  (comp, eval) <- getPathMaps
  forM_ roots $ joinEach comp eval

joinEach :: CompMap -> EvalMap -> FilePath -> IO ()
joinEach comp eval root = do
  dotLinksPaths <- catCallContents root getDotLinksPaths
  linkss <- forM dotLinksPaths $ adjustConsumeLinksInFile eval root
  let links = concat linkss
  appendLinksIn comp links root



getDotLinksPaths :: FilePath -> IO [FilePath]
getDotLinksPaths root = do
  isFile <- doesFileExist root      -- there is nothing we can do with a file
  isLink <- pathIsSymbolicLink root -- on one hand we don't want loops, 
                                    -- on the other the target could be intended as separate
  if isFile || isLink
    then return []
    else do
      isBlocked <- doesFileExist $ root </> "block" <.> "links"
      if isBlocked
        then return []
        else do
          mDotLinks <- getDotLinkIn root
          case mDotLinks of
            Just dotLinks -> return [dotLinks]
            Nothing -> catCallContents root getDotLinksPaths

catCallContents :: FilePath -> (FilePath -> IO [a]) -> IO [a]
catCallContents path action = do
  contents <- listDirectory path
  let absContents = map (normalise . (path </>)) contents
  resultss <- mapM action absContents
  return $ concat resultss
