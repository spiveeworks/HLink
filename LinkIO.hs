module LinkIO
( Link(Link)
, readLink
, showLink

, applyLinkDest
) where

import Data.Char (isSpace)
import Data.List (intercalate)
import Data.List.Split (splitOn)

import System.FilePath (splitDirectories)

data Link = Link FilePath FilePath -- basically (source, dest)


readLink :: String -> Link
readLink x = case map (dropWhile isSpace) (splitOn ":" x) of
               [extern] -> Link extern "."
               [extern, intern] -> Link extern intern

showLink :: Link -> String
showLink = intercalate ": " . getList
  where getList (Link extern intern) 
          | splitDirectories intern == ["."] = [extern]
          | intern == "" = [extern]
          | otherwise = [extern, intern]


applyLinkDest :: (FilePath -> FilePath) -> Link -> Link
applyLinkDest f (Link source dest) = Link source (f dest)

