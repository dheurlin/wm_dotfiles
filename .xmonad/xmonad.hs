{-# LANGUAGE LambdaCase #-}

import Dmenu
import Vars

import XMonad
import XMonad.Core
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig

import Data.List
import Control.Monad
import System.Exit

main = xmonad =<< xmobar (desktopConfig
  { terminal = myTerminal
  , modMask = mod4Mask
  } `additionalKeysP` myBindings)

myBindings :: [(String, X ())]
myBindings =
  [ ("M-S-q", confirm "Quit XMonad?" $ liftIO (exitWith ExitSuccess))
  , ("M-q"  , confirm "Recompile and restart XMonad?"
                $ spawn "xmonad --recompile && xmonad --restart")
  , ("M-p"  , dmenuRun)
  ]
