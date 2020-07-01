module Bindings where

import Navigation
import Dmenu

import XMonad

import System.Exit

myBindings :: [(String, X ())]
myBindings =
  [ -- XMonad stuff -------------------------------------------------
    ("M-S-q", confirm "Quit XMonad?" $ liftIO exitSuccess           )
  , ("M-q"  , confirm "Restart XMonad?"   $ spawn "xmonad --restart")
  , ("M-S-r", confirm "Recompile and restart XMonad?"
                $ spawn "xmonad --recompile && xmonad --restart"    )
  , ("M-p"  , dmenuRun)
  , ("M1-j" , windows prevWS)
  , ("M1-k" , windows nextWS)
  -- Keyboard layout ----------------------------------------------------------
  , ("M-M1-<Space>", spawn "xkb-switch -n"     ) -- toggle between layouts
  , ("C-<Esc>"  , spawn "xdotool key Caps_Lock")
  -- Media Keys ---------------------------------------------------------------
  , ("<XF86AudioRaiseVolume>"    , spawn "~/.config/scripts/volume.sh 5%+")
  , ("<XF86AudioLowerVolume>"    , spawn "~/.config/scripts/volume.sh 5%-")
  , ("<XF86AudioMute>"           , spawn "amixer set Master toggle"       )
  , ("<XF86MonBrightnessUp>"     , spawn "xbacklight -inc 10"             )
  , ("<XF86MonBrightnessDown>"   , spawn "xbacklight -dec 10"             )
  , ("M-<F10>"                   , spawn "xbacklight -set 1"              )
  , ("S-<XF86MonBrightnessUp>"   , spawn "~/bin/redshift-ctrl inc"        )
  , ("S-<XF86MonBrightnessDown>" , spawn "~/bin/redshift-ctrl dec"        )
  , ("<XF86AudioPlay>"           , spawn "playerctl play-pause"           )
  , ("<XF86AudioNext>"           , spawn "playerctl next"                 )
  , ("<XF86AudioPrev>"           , spawn "playerctl previous"             )
  -- Misc ---------------------------------------------------------------------
  , ("<Print>"  , spawn "gnome-screenshot"         )
  , ("S-<Print>", spawn "gnome-screenshot -a"      )
  , ("M-M1-l"   , spawn "~/.config/scripts/lock.sh")
  ] <> [
  -- Application shortcutsl ---------------------------------------------------
    ("M-C-" <> key, spawn program) |
      (key, program) <- [ ("b", "firefox-nightly")
                        , ("n", "nautilus")
                        , ("r", "kitty zsh -c -i 'ranger'")
                        , ("c", "gnome-calculator")
                        ]
  ]

