module Util where

import Data.Char

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
