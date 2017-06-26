module LinkIO
( Link
, readLink
, showLink
) where

import Data.Char (isSpace)
import Data.List (intercalate)
import Data.List.Split (splitOn)

import System.FilePath (splitDirectories)

data Link = Link FilePath FilePath -- basically (source, dest)

instance Functor Link where
  fmap f (Link source dest) = Link source (f dest)

readLink x = case map (dropWhile isSpace) (splitOn ":" x) of
               [extern] -> Link extern "."
               [extern, intern] -> Link extern intern

showLink = intercalate ": " . getList
  where getList (Link extern intern) 
          | splitDirectories intern == ["."] = [extern]
          | intern == "" = [extern]
          | otherwise = [extern, intern]


