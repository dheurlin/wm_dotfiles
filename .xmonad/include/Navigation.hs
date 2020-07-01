module Navigation where

import XMonad
import qualified XMonad.StackSet as SS

import Data.List
import Data.Maybe

-- | Focuses the previous workspace
prevWS :: WindowSet -> WindowSet
prevWS = relWS (-1)

-- | Focuses the next workspace
nextWS :: WindowSet -> WindowSet
nextWS = relWS 1

-- | Focuses a workspace with an index relative to the current one
relWS :: Int -> WindowSet -> WindowSet
relWS rel ws = SS.view newIx ws
 where
  curWS  = SS.currentTag ws
  allWS  = sort.map SS.tag $ SS.workspaces ws
  ix     = fromJust $ curWS `elemIndex` allWS
  bounds = min (length allWS - 1) . max 0
  newIx  = allWS !! (bounds $ ix + rel)


