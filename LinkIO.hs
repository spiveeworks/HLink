module LinkIO
( RelativeLink((:->:))
, Link((:=>:))

, evaluateLink
, compressLink

, dotLinkSeparator
, prettyDotLinkSeparator
, readLink
, showLink
, parseLinks
, formatLinks
) where

import Data.Char (isSpace)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

import System.FilePath

import PathCompression

data RelativeLink = FilePath' :->: FilePath -- compressed source and relative dest
data Link         = FilePath  :=>: FilePath -- valid source and absolute destination

------ Link Conversion

evaluateLink :: EvalMap -> FilePath -> RelativeLink -> Link
evaluateLink eval cd (source :->: dest) = evaluatePath eval source :=>: makeAbsoluteWith cd dest
  where makeAbsoluteWith cd path = normalise path'
          where path' = if isRelative path then cd </> path
                                           else path

compressLink :: CompMap -> FilePath -> Link -> RelativeLink
compressLink comp cd (source :=>: dest) = compressPath comp source :->: makeRelative cd dest

------ Link Reading/Writing

---- Separator

dotLinkSeparator :: String
dotLinkSeparator = "->"

prettyDotLinkSeparator :: String
prettyDotLinkSeparator = pretty dotLinkSeparator
  where pretty str = " " ++ str ++ " "

---- String <-> RelativeLink

readLink :: String -> Maybe RelativeLink
readLink x = case map trim (splitOn dotLinkSeparator x) of
               ("" : _) -> Nothing
               [extern] -> Just $ extern :->: "."
               [extern, intern] -> Just $ extern :->: intern
               _ -> Nothing
  where trim = revDrop . revDrop
        revDrop = reverse . dropWhile isSpace

showLink :: RelativeLink -> String
showLink = intercalate (prettyDotLinkSeparator) . getList
  where getList (extern :->: intern) 
          | intern `equalFilePath` "." = [extern]
          | intern == "" = [extern]
          | otherwise = [extern, intern]

---- String <-> [Link]

parseLinks :: EvalMap -> FilePath -> String -> [Link]
parseLinks eval cd = map (evaluateLink eval cd) . mapMaybe readLink . lines

formatLinks :: CompMap -> FilePath -> [Link] -> String
formatLinks comp cd = unlines . map showLink . map (compressLink comp cd)

