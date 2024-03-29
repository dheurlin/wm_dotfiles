module Bindings where

import           Navigation
import           Dmenu
import           Vars

import           XMonad                  hiding ( (|||) )
import           XMonad.Layout.LayoutCombinators
import qualified XMonad.StackSet               as SS
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.Spacing
import           XMonad.Util.Scratchpad ( scratchpadSpawnActionCustom )

import           System.Exit
import BarNotifications (showVolumeNotification)

myBindings :: [(String, X ())]
myBindings =
  [ -- XMonad stuff -------------------------------------------------
    ("M-b"  , sendMessage ToggleStruts                              )
  , ("M-S-q", confirm "Quit XMonad?" $ liftIO exitSuccess           )
  , ("M-q"  , confirm "Restart XMonad?"   $ spawn "xmonad --restart")
  , ("M-S-r", confirm "Recompile and restart XMonad?"
                $ spawn "xmonad --recompile && xmonad --restart"    )
  , ("M-p"  , spawn "rofi -show combi")
  , ("M-S-p", spawn "~/.config/rofi/cmd.sh")
  , ("M1-j" , windows prevWS)
  , ("M1-k" , windows nextWS)
  , ("M-0"  , windows $ SS.view  "10")
  , ("M-S-0", windows $ SS.shift "10")
  , ("M-m"  , windows $ SS.view  "(music)")
  , ("M-s"  , windows $ SS.view  "(messaging)")
  , ("M-n"  , windows $ \ws -> SS.view  (getNewWS ws) ws)
  , ("M-S-n", windows $ \ws -> SS.shift (getNewWS ws) ws)
  , ("M-S-o", windows $ \ws -> let new = getNewWS ws
                               in SS.view new . SS.shift new $ ws)
  , ("M-f"  , sendMessage $ JumpToLayout "Full")
  -- Spawn scratchpad terminal
  , ("M-C-<Return>",
     scratchpadSpawnActionCustom $ myTerminal <> " --name scratchpad")
  -- Toggle gaps
  , ("M-g"  , toggleScreenSpacingEnabled >> toggleWindowSpacingEnabled )
  , ("M-S-g", toggleSmartSpacing ) -- toggle smart gaps
  -- Keyboard layout ----------------------------------------------------------
  , ("M-M1-<Space>", spawn "xkb-switch -n"        ) -- toggle between layouts
  , ("C-<Esc>"     , spawn "xdotool key Caps_Lock")
  -- Media Keys ---------------------------------------------------------------
  , ("<XF86AudioRaiseVolume>"    , spawn "pulseaudio-control --volume-max 130 up" >> showVolumeNotification)
  , ("<XF86AudioLowerVolume>"    , spawn "pulseaudio-control --volume-max 130 down" >> showVolumeNotification)
  , ("<XF86AudioMute>"           , spawn "pulseaudio-control togmute"       )
  -- , ("<XF86MonBrightnessUp>"     , spawn "xbacklight -inc 10"             )
  -- , ("<XF86MonBrightnessDown>"   , spawn "xbacklight -dec 10"             )
  , ("M-<F10>"                   , spawn "xbacklight -set 1"              )
  -- , ("S-<XF86MonBrightnessUp>"   , spawn "~/bin/redshift-ctrl inc"        )
  -- , ("S-<XF86MonBrightnessDown>" , spawn "~/bin/redshift-ctrl dec"        )
  , ("<XF86AudioPlay>"           , spawn "playerctl play-pause"           )
  , ("<XF86AudioNext>"           , spawn "playerctl next"                 )
  , ("<XF86AudioPrev>"           , spawn "playerctl previous"             )
  -- Misc ---------------------------------------------------------------------
  , ("<Print>"     , spawn "scrot \"$HOME/Bilder/screenshots/screenshot_$(date +%Y_%m_%d_%H:%M:%S).png\"")
  , ("S-<Print>"   , spawn "scrot -s \"$HOME/Bilder/screenshots/screenshot_$(date +%Y_%m_%d_%H:%M:%S).png\"")
  , ("M-M1-l"      , spawn "~/.config/scripts/lock.sh")
  -- , ("M-M1-l"      , spawn "blurlock")
  , ("C-S-<Space>"   , spawn "dunstctl close")
  , ("M-C-<Space>" ,
      spawn "/home/danielheurlin/scripts/dmenu-unicode/dmenu-unicode.sh")
  ] <> [
  -- Application shortcuts  ---------------------------------------------------
    ("M-C-" <> key, spawn program) |
      (key, program) <- [ ("b", "firefox")
                        , ("n", "pcmanfm")
                        , ("r", "kitty zsh -c -i 'ranger'")
                        , ("c", "gnome-calculator")
                        ]
  ] <> concat [
  -- Swapping workspaces ------------------------------------------------------
    [ ("M-M1-"   <> targ, windows $ swapWs  targ) -- swap, keep focus on target
    , ("M-M1-C-" <> targ, windows $ swapWs' targ) -- swap, keep focus on current
    ] | targ <- show <$> [1..10]
  ]

