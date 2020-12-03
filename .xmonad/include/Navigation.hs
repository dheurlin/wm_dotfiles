module Navigation where

import           XMonad
import qualified XMonad.StackSet               as SS

import           Data.List
import           Data.Char
import           Data.Maybe
import           Control.Arrow                  ( (>>>) )
import           Text.Read                      ( readMaybe )

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
    sort $ filter (\name -> not (any isNumber name) && name /= "NSP")
      . map SS.tag $ SS.workspaces ws
  allWS  = allNumeric <> allSpecial
  curWS  = SS.currentTag ws
  ix     = fromJust $ curWS `elemIndex` allWS
  bounds = min (length allWS - 1) . max 0
  newIx  = allWS !! bounds (ix + rel)

-- | Returns the tag of an empty workspace one exists, otherwise return the
-- current workspace tag.
getNewWS :: WindowSet -> WorkspaceId
getNewWS ws = maybe (SS.currentTag ws) (show . SS.tag) $ find isEmpty numeric
 where numeric = sortOn SS.tag $ numericWorkspaces ws

-- | Swaps the current workspace with the workspace having the given tag
swapWs :: WorkspaceId -> WindowSet -> WindowSet
swapWs target ws = swapWs' ws
  where swapWs' = SS.renameTag curr "swap"   >>>
                  SS.renameTag target curr   >>>
                  SS.renameTag "swap" target

        curr    = SS.currentTag ws

-- | like swapWs, but keeping focus on the current tag ("sending off" the ws)
swapWs' :: WorkspaceId -> WindowSet -> WindowSet
swapWs' target ws = SS.view (SS.currentTag ws) . swapWs target $ ws

isNonEmpty :: SS.Workspace i l a -> Bool
isNonEmpty = isJust . SS.stack

isEmpty :: SS.Workspace i l a -> Bool
isEmpty = isNothing . SS.stack

-- | Gets a list of all numeric workspaces in the WindowSet,
-- with the numbers parsed into Ints
numericWorkspaces :: WindowSet -> [SS.Workspace Int (Layout Window) Window]
numericWorkspaces = catMaybeTags . mapTags readMaybe . SS.workspaces

catMaybeTags :: [SS.Workspace (Maybe i) l a] -> [SS.Workspace i l a]
catMaybeTags ws =
  concat [ maybe [] (\x -> [w { SS.tag = x }]) t | w <- ws, let t = SS.tag w ]

-- | Maps a function over the tags of a list of workspaces
mapTags :: (i -> j) -> [SS.Workspace i l a] -> [SS.Workspace j l a]
mapTags f [] = []
mapTags f ((SS.Workspace t l s) : tags) =
  SS.Workspace (f t) l s : mapTags f tags
