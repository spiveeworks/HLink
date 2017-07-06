import Data.Function(on)
import Data.List.Split(splitOn)

import Control.Monad

import System.Directory
import System.IO
import System.FilePath


findPaths :: FilePath -> IO [FilePath]
findPaths path
  | path `endsWith` ".links" = return [path]
  | otherwise = listDirectory' path >>= liftM concat . mapM findPaths . map (path</>)
  where endsWith = (==) `on` (take 6 . reverse)

listDirectory' :: FilePath -> IO [FilePath]
listDirectory' path = do
  isFile <- doesFileExist path
  if isFile then return []
            else listDirectory path


fixPath :: FilePath -> IO ()
fixPath path = do
  str <- readFile path
  let str' = unlines . map fixLine . lines $ str
  length str' `seq` writeFile path str'

fixLine :: String -> String
fixLine str = if hasDrive ex || head ex `elem` ".~"
                 then ex
                 else '$': ex
  where (ex : _) = splitOn "->" str

