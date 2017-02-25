module LinkIO
( Link
, readLink
, showLink
) where

import Data.Char (isSpace)
import Data.List (intercalate)
import Data.List.Split (splitOn)


data Link = Root FilePath | Relative FilePath FilePath | Switch String FilePath FilePath

readLink x = case map (dropWhile isSpace) (splitOn ":" x) of
               [extern] -> Root extern
               [extern, intern] -> Relative extern intern
               [name, extern, switchloc] -> Switch name extern switchloc

showLink = intercalate ": " . getList
  where getList (Root extern) = [extern]
        getList (Relative extern intern) = [extern, intern]
        getList (Switch name extern switchloc) = [name, extern, switchloc]


