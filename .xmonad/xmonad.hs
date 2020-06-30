{-# LANGUAGE LambdaCase #-}

import Dmenu
import Vars
import qualified Colors as Col

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig

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
  ]

myXmobar = statusBar ("xmobar " <> opts ) myXmobarPP toggleStrutsKey
 where opts = unwords [ "-F", "gray"
                      , "-B", show Col.bg
                      , "-f", show $ "xft:" <> font
                      ]

myXmobarPP =
  xmobarPP { ppCurrent = xmobarColor "white" Col.accentBg . wrap " " " "
           }

-- ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
--                      , ppTitle   = xmobarColor "green"  "" . shorten 40

-- | Binding to toggle xmobar gap
toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )
