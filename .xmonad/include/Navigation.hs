module Navigation where

import           XMonad
import qualified XMonad.StackSet               as SS

import           Data.List
import           Data.Char
import           Data.Maybe

-- | Focuses the previous numeric workspace
prevWS :: WindowSet -> WindowSet
prevWS = relWS (-1)

-- | Focuses the next numeric workspace
nextWS :: WindowSet -> WindowSet
nextWS = relWS 1

-- | Focuses a numeric workspace with an index relative to the current one
relWS :: Int -> WindowSet -> WindowSet
relWS rel ws = SS.view newIx ws
 where
  curWS = SS.currentTag ws
  allWS =
    sortOn (read :: String -> Int)
      . filter (all isNumber)
      . map SS.tag
      $ SS.workspaces ws
  ix     = fromJust $ curWS `elemIndex` allWS
  bounds = min (length allWS - 1) . max 0
  newIx  = allWS !! bounds (ix + rel)


