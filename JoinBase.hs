module JoinBase
( adjustConsumeLinksInFile
, adjustConsumeLinksInSymLink
) where

import System.Directory
import System.FilePath

import PathCompression(EvalMap)
import LinkParsing( Link((:=>:)) )
import qualified LinkIO

-- When dealing with a physical directory, use the path as both link and target
-- Basis of the Join action
adjustConsumeLinksInFile :: EvalMap -> FilePath -> FilePath -> IO [Link]
adjustConsumeLinksInFile eval newRoot dotLinks = adjustConsumeLinks eval newRoot oldRoot oldRoot dotLinks
  where oldRoot = takeDirectory dotLinks


-- When dealing with a symlink, use the target for interpreting impossible links
-- This is basically Pull's entire functionality
adjustConsumeLinksInSymLink :: EvalMap -> FilePath -> FilePath -> FilePath -> IO [Link]
adjustConsumeLinksInSymLink eval newRoot oldRoot oldTarget = do
  mDotLinks <- LinkIO.getDotLinkIn oldRoot
  case mDotLinks of
    Just dotLinks -> adjustConsumeLinks eval newRoot oldRoot oldTarget dotLinks
    Nothing -> return []


-- Deletes a .links file, but gives you its contents.
-- Also updates any relative source paths
adjustConsumeLinks :: EvalMap -> FilePath -> FilePath -> FilePath -> FilePath -> IO [Link]
adjustConsumeLinks eval newRoot oldRoot oldTarget dotLinks = do
  links <- LinkIO.getLinksWith eval oldRoot dotLinks
  length links `seq` removeFile dotLinks
  return $ map adjust links
  where adjust = adjustLink newRoot oldRoot oldTarget


-- Deals with relative sources, updating them to the new root directory
-- Dest cannot be inside source, so use another path "oldTarget" to interpret
adjustLink :: FilePath -> FilePath -> FilePath -> Link -> Link
adjustLink newRoot oldRoot oldTarget link@(source :=>: dest)
  | isAbsolute source = link  -- nothing to change, working with an explicit link
  | equalFilePath dest destFromSource = sourceFromRoot :=>: dest  -- not trying to put target inside source
  | otherwise = sourceFromRoot :=>: destViaTarget  -- special case
  where destFromSource = makeRelative (oldRoot </> source) dest
        sourceFromRoot = normalise $ deltaRoot </> source
        destViaTarget = normalise . (oldTarget </>) . makeRelative oldRoot $ dest
        deltaRoot = makeRelative newRoot $ oldRoot  -- path to old root from new root

