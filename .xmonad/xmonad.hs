{-# LANGUAGE LambdaCase #-}

import Util
import Dmenu
import Vars
import qualified Colors as Col

import XMonad
import qualified XMonad.StackSet as SS
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig

import Data.List
import Data.Maybe

import System.Exit

main = xmonad =<< myXmobar (desktopConfig
  { terminal = myTerminal
  , modMask = mod4Mask
  } `additionalKeysP` myBindings)

myBindings :: [(String, X ())]
myBindings =
  [ ("M-S-q", confirm "Quit XMonad?" $ liftIO (exitWith ExitSuccess))
  , ("M-q"  , confirm "Restart XMonad?"   $ spawn "xmonad --restart")
  , ("M-S-r", confirm "Recompile and restart XMonad?"
                $ spawn "xmonad --recompile && xmonad --restart")
  , ("M-p"  , dmenuRun)
  , ("M1-j" , windows prevWS)
  , ("M1-k" , windows nextWS)
  ]

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


-- | XMobar setup
myXmobar = statusBar ("xmobar " <> opts ) myXmobarPP toggleStrutsKey
 where opts = unwords [ "-F", "gray"
                      , "-B", show Col.bg
                      , "-f", show $ "xft:" <> font
                      ]

myXmobarPP =
  xmobarPP { ppCurrent = xmobarColor "white" Col.accentBg . wrap " " " "
           }

-- | Binding to toggle xmobar gap
toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )
