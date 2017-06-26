import System.Environment
import System.IO
import System.FilePath

import Control.Monad

import PathCompression
import LinkIO

main = do
  (comp, eval) <- getPathMaps
  args <- getArgs
  forM args $ mountEach eval

mountEach :: EvalMap -> FilePath -> IO ()
mountEach eval path = do
  contents <- readFile (path </> "" <.> "links")
  let instructions = parse eval contents
  forM instructions attemptLink

parse :: EvalMap -> String -> [Link]
parse eval = map (fmap doEval) . map readLink . lines
  where doEval = evaluatePath eval


-- Creates a symbolic link as requested, unless something other than a symbolic link is in the way.
attemptLink :: Link -> IO ()
attemptLink (Link source dest) = do
  is_used <- doesPathExist source
  is_available <- if is_used
                    then clearIfRemovableAndPrint source
                    else return True
  when is_available $ do
    createDirectoryLink dest source
    putStrLn $ "Successfully created link at " ++ source
 where clearIfRemovableAndPrint source = do
         m_old_dest <- clearIfRemovable
         putStrLn $ case m_old_dest of
           Just old_dest -> "Removed old link: " ++ source ++ " -> " ++ old_dest
           Nothing -> "Failed to create link at: " ++ source


-- Removes symbolic links and reports their target, does not remove actual files/directories
clearIfRemovable :: FilePath -> String -> String -> IO (Maybe FilePath)
clearIfRemovable path rem_msg non_rem_msg = do
  result <- pathIsSymbolicLink path
  if result
    then do old_dest <- getSymbolicLinkTarget path
            removeLink path
            return (Just old_dest)
    else return Nothing

-- removes directory and file links portably
removeLink :: FilePath -> IO ()
removeLink path = do
  is_dir <- doesDirectoryExist path
  if is_dir
    then removeDirectoryLink path
    else removeFile path

      
