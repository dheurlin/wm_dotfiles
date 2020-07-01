import Vars
import Bindings ( myBindings )
import qualified Colors as Col

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig
import XMonad.Layout.NoBorders

main = xmonad =<< myXmobar (desktopConfig
  { terminal = myTerminal
  , modMask = mod4Mask
  , layoutHook = myLayout
  , normalBorderColor = Col.unFocusedBorder
  , focusedBorderColor = Col.focusedBorder
  } `additionalKeysP` myBindings)

-- XMobar setup ---------------------------------------------------------------
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


-- Layouts --------------------------------------------------------------------
myLayout =  tiled ||| Mirror tiled ||| noBorders Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = smartBorders $ Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
