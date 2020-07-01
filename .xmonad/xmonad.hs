import Vars
import Bindings ( myBindings )
import qualified Colors as Col

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig

main = xmonad =<< myXmobar (desktopConfig
  { terminal = myTerminal
  , modMask = mod4Mask
  } `additionalKeysP` myBindings)

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
