module Navigation where

import           XMonad
import qualified XMonad.StackSet               as SS

import           Data.List
import           Data.Char
import           Data.Maybe

-- | Focuses the previous special or nonempty numeric workspace
prevWS :: WindowSet -> WindowSet
prevWS = relWS (-1)

-- | Focuses the next special or nonempty numeric workspace
nextWS :: WindowSet -> WindowSet
nextWS = relWS 1

-- | Focuses a special or numeric nonempty workspace
--  with an index relative to the current one
relWS :: Int -> WindowSet -> WindowSet
relWS rel ws = SS.view newIx ws
 where
  isCurrent w = SS.tag w == curWS
  allNumeric =
    sortOn (read :: String -> Int)
      . filter (all isNumber)
      . map SS.tag
      . filter (\ws -> isCurrent ws || isNonEmpty ws)
      $ SS.workspaces ws
  allSpecial =
    sort $ filter (not . any isNumber) . map SS.tag $ SS.workspaces ws
  allWS  = allNumeric <> allSpecial
  curWS  = SS.currentTag ws
  ix     = fromJust $ curWS `elemIndex` allWS
  bounds = min (length allWS - 1) . max 0
  newIx  = allWS !! bounds (ix + rel)

-- | Returns the tag of an empty workspace one exists, otherwise return the
-- current workspace tag.
getNewWS :: WindowSet -> WorkspaceId
getNewWS ws = maybe (SS.currentTag ws) SS.tag $ find isEmpty numeric
  where numeric = filter (all isNumber . SS.tag) $ SS.workspaces ws

isNonEmpty :: SS.Workspace i l a -> Bool
isNonEmpty = isJust . SS.stack

isEmpty :: SS.Workspace i l a -> Bool
isEmpty = isNothing . SS.stack
