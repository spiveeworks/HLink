module LinkIO
( dotLinkSeparator
, prettyDotLinkSeparator

, Link(Link)
, readLink
, showLink

, mapLinkSource
) where

import Data.Char (isSpace)
import Data.List (intercalate)
import Data.List.Split (splitOn)

import System.FilePath (splitDirectories)

data Link = Link FilePath FilePath -- basically (source, dest)

dotLinkSeparator :: String
dotLinkSeparator = "->"

prettyDotLinkSeparator :: String
prettyDotLinkSeparator = pretty dotLinkSeparator
  where pretty str = " " ++ str ++ " "

readLink :: String -> Maybe Link
readLink x = case map trim (splitOn dotLinkSeparator x) of
               ("" : _) -> Nothing
               [extern] -> Just $ Link extern "."
               [extern, intern] -> Just $ Link extern intern
               _ -> Nothing
  where trim = revDrop . revDrop
        revDrop = reverse . dropWhile isSpace

showLink :: Link -> String
showLink = intercalate (prettyDotLinkSeparator) . getList
  where getList (Link extern intern) 
          | splitDirectories intern == ["."] = [extern]
          | intern == "" = [extern]
          | otherwise = [extern, intern]


mapLinkSource :: (FilePath -> FilePath) -> Link -> Link
mapLinkSource f (Link source dest) = Link (f source) dest

