import           Vars
import           Bindings                       ( myBindings )
import qualified Colors                        as Col

import           XMonad
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicLog
import           XMonad.Util.EZConfig
import           XMonad.Layout.NoBorders
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Util.SpawnOnce

import           Control.Monad
import           Data.Maybe

main = xmonad =<< myXmobar
  (                 desktopConfig
      { terminal           = myTerminal
      , modMask            = mod4Mask
      , layoutHook         = myLayout
      , normalBorderColor  = Col.unFocusedBorder
      , focusedBorderColor = Col.focusedBorder
      , handleEventHook = handleEventHook desktopConfig <> fullscreenEventHook
      , manageHook         = myManageHook
      , startupHook        = startupHook desktopConfig
                             >> addEWMHFullscreen
                             >> myStartupItems
      }
  `additionalKeysP` myBindings
  )

-- Startup items --------------------------------------------------------------
myStartupItems :: X ()
myStartupItems = sequence_ [ spawnTrayer ]

spawnTrayer :: X ()
spawnTrayer = do
  spawnOnce $ "trayer " <> trayeropts
  -- Put trayer below fullscreen windows
  spawn "xdo above -t \"$(xdo id -n xmobar)\" \"$(xdo id -N trayer -m)\""
 where
   trayeropts = unwords [ "--widthtype"    , "request"
                        , "--align"        , "right"
                        , "--height"       , "14"
                        , "--edge"         , "top"
                        , "--distancefrom" , "right"
                        , "--distance"     , "652"
                        , "--transparent"  , "true"
                        , "--alpha"        , "0"
                        , "--tint"         , show $ "0x" <> tail Col.bg
                        , "--iconspacing"  , "7"
                        ]

-- XMobar setup ---------------------------------------------------------------
myXmobar = statusBar ("xmobar " <> opts) myXmobarPP toggleStrutsKey
 where
  opts = unwords ["-F", "gray", "-B", show Col.bg, "-f", show $ "xft:" <> font]

myXmobarPP =
  xmobarPP { ppCurrent = xmobarColor "white" Col.accentBg . wrap " " " " }

-- | Binding to toggle xmobar gap
toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )


-- Layouts --------------------------------------------------------------------
myLayout = tiled ||| Mirror tiled ||| noBorders Full
 where
   -- default tiling algorithm partitions the screen into two panes
  tiled   = smartBorders $ Tall nmaster delta ratio

  -- The default number of windows in the master pane
  nmaster = 1

  -- Default proportion of screen occupied by master pane
  ratio   = 1 / 2

  -- Percent of screen to increment by when resizing panes
  delta   = 3 / 100

-- Manage Hooks ---------------------------------------------------------------
myManageHook :: ManageHook
myManageHook = mconcat [manageDocks]


-- Add support for NET_WM_FULLSCREEN ------------------------------------------
addNETSupported :: Atom -> X ()
addNETSupported x = withDisplay $ \dpy -> do
  r               <- asks theRoot
  a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
  a               <- getAtom "ATOM"
  liftIO $ do
    sup <- join . maybeToList <$> getWindowProperty32 dpy a_NET_SUPPORTED r
    when (fromIntegral x `notElem` sup) $ changeProperty32 dpy
                                                           r
                                                           a_NET_SUPPORTED
                                                           a
                                                           propModeAppend
                                                           [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen = do
  wms <- getAtom "_NET_WM_STATE"
  wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
  mapM_ addNETSupported [wms, wfs]
