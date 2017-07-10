module JoinBase(main) where

-- When dealing with a physical directory, use the path as both link and target
adjustConsumeLinksInDir :: EvalMap -> FilePath -> FilePath -> IO [Link]
adjustConsumeLinksInDir eval newRoot oldRoot = adjustConsumeLinks eval newRoot oldRoot oldRoot

-- Deletes a .links file, but gives you its contents.
-- Also updates any relative source paths
-- Basis of the Join action
adjustConsumeLinks :: EvalMap -> FilePath -> FilePath -> FilePath -> IO [Link]
adjustConsumeLinks eval newRoot oldRoot oldTarget = do
  mDotLinks <- getDotLinkIn oldRoot
  case mDotLinks of
    Just dotLinks -> do
      links <- getLinksWith eval oldRoot dotLinks
      length links `seq` removeFile dotLinks
      return $ map adjust links
    Nothing -> return []
  where adjust = adjustLink newRoot oldRoot oldTarget

-- Deals with relative sources, updating them to the new root directory
-- If the dest is inside the source then assume the new root is a link, and put new dest inside that link's target.
adjustLink :: FilePath -> FilePath -> FilePath -> Link -> Link
adjustLink newRoot oldRoot oldTarget link@(source :=>: dest)
  | isAbsolute source = link  -- nothing to change, working with an explicit link
  | equalFilePath dest destFromSource = sourceFromRoot :=>: dest  -- not trying to put target inside source
  | otherwise = sourceFromRoot :=>: destViaTarget  -- special case
  where destFromSource = makeRelative (root </> source) dest
        sourceFromRoot = normalise $ deltaRoot </> source
        destViaTarget = normalise . (oldTarget </>) . makeRelative root $ dest
        deltaRoot = makeRelative root $ takeDirectory dotLinks  -- path to old root from new root


