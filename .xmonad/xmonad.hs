{-# LANGUAGE FlexibleContexts  #-}

import           Vars
import           Util                           ( doAbsRectFloat )
import           Bindings                       ( myBindings )
import qualified Colors                        as Col

import           XMonad                  hiding ( (|||) )
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.Named
import           XMonad.Config.Desktop
import           XMonad.Hooks.ManageHelpers
import qualified XMonad.StackSet               as SS
import           XMonad.StackSet                ( floating
                                                , screenDetail
                                                )
import           XMonad.Util.EZConfig
import           XMonad.Util.SpawnOnce
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Grid
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.WindowSwallowing
import           XMonad.Util.Run
import           XMonad.Util.Scratchpad ( scratchpadManageHookDefault )
import           XMonad.Hooks.DynamicProperty
import           XMonad.Layout.Spacing

import           Text.Read                      ( readMaybe )
import           Control.Monad
import           Data.Maybe
import           Data.List
import           Data.Semigroup
import           Data.Functor                   ( ($>) )
import           Data.Function                  ( on )
import qualified Data.Map                      as M
import XMonad.Hooks.SetWMName

main = do
  forM_ [["/tmp/.xmonad-layout-log"],[ "/tmp/.xmonad-workspace-log"]] $ safeSpawn "mkfifo"
  xmonad $ setEwmhActivateHook doFocus $ ewmhFullscreen $ docks $ ewmh
    ( desktopConfig
        { terminal           = myTerminal
        , modMask            = mod4Mask
        , workspaces         = map show [1..10] <> ["(messaging)", "(music)"]
        , layoutHook         = avoidStruts myLayout
        , borderWidth        = 1
        , normalBorderColor  = Col.unFocusedBorder
        , focusedBorderColor = Col.focusedBorder
        , handleEventHook    = handleEventHook desktopConfig <> myHandleEventHook
        , manageHook         = myManageHook
        , logHook            = eventLogHookForPolybar *> setWMName "LG3D"
        , startupHook        = startupHook desktopConfig
                               >> addEWMHFullscreen
                               >> myStartupItems
        }
      `additionalKeysP` myBindings
    )

-- Startup items --------------------------------------------------------------

myStartupItems :: X ()
myStartupItems = do
  spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
  spawnOnce "nitrogen --restore; sleep 1; picom -b"
  spawnOnce "nm-applet"
  spawnOnce "xfce4-power-manager"
  -- spawnOnce "volumeicon"
  spawnOnce "pamac-tray"
  spawnOnce "clipit"
  spawnOnce "blueman-applet"
  spawnOnce "ff-theme-util"
  spawn     "xinput --set-prop 'DLL082A:01 06CB:76AF Touchpad' 'libinput Accel Speed' .4; xset m 15/10 0"
  spawnOnce "xss-lock -- blurlock"
  spawnOnce "/home/danielheurlin/.config/scripts/keepAwakeWhilePlaying.sh"
  spawnOnce "/home/danielheurlin/bin/libinput-three-finger-drag"
  spawn     "/home/danielheurlin/.config/scripts/kbd-setup.sh"
  spawnOnce "/home/danielheurlin/.config/polybar/launch.sh"
  spawnOnce $ "alttab " <> alttabFlags
  where
    alttabFlags = unwords
      [ "-w 1"
      , "-d 1"
      , "-pk Left"
      , "-nk Right"
      , "-vp pointer"
      , "-t 128x150"
      , "-i 127x64"
      , "-font", show "xft: Source Code Pro-8"
      ]


-- Bar setup     ---------------------------------------------------------------

-- this creates a FIFO containing the current layout which polybar can read from
eventLogHookForPolybar :: X ()
eventLogHookForPolybar = do
  winset <- gets windowset
  let ld = description . SS.layout . SS.workspace . SS.current $ winset
  let currWs = SS.currentTag winset
  let visWs  = map (SS.tag . SS.workspace) $ SS.visible winset
  let wss    = [ (t, SS.tag t) | t <- SS.workspaces winset ]

  io $ appendFile "/tmp/.xmonad-layout-log" (ld ++ "\n")
  io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr currWs visWs wss ++ "\n")

  where
    mkIcon "(music)"     = "\60579"
    mkIcon "(messaging)" = "\61364"
    mkIcon ws = ws

    activeMarker ws
      | isJust $ SS.stack ws = "'"
      | otherwise            = " "

    fmt currWs visWs (wst, ws)
          | currWs == ws              = "%{F#292F34}%{B#16a085} "     ++ mkIcon ws ++  " %{F-}%{B-}"
          | ws `elem` visWs           = "%{u#555}%{+u} " ++ mkIcon ws ++ " %{u-}%{-u}"
          | isNothing $ SS.stack wst  = "%{F#888} "      ++ mkIcon ws ++ " %{F-}"
          | otherwise                 = " "              ++ mkIcon ws ++ " "

    wsStr currWs visWs wss = concatMap (fmt currWs visWs) . sortBy (compWss `on` snd) $  wss

    rm :: String -> Maybe Int
    rm = readMaybe

    compWss "(messaging)" "(music)" = LT
    compWss "(music)" "(messaging)" = GT
    compWss a b
      | (Just n) <- rm a, (Just m) <- rm b = compare n m
      | (Just n) <- rm a, Nothing  <- rm b = LT
      | Nothing  <- rm a, (Just _) <- rm b = GT
      | otherwise = EQ


-- Layouts --------------------------------------------------------------------
myLayout = lessBorders AllFloats $
              named "Tall"         (gaps tiled)
          ||| named "Mirror tall"  (gaps $ Mirror tiled)
          ||| named "Grid"         (gaps Grid)
          ||| named "Full"         (noBorders Full)
 where
   -- default tiling algorithm partitions the screen into two panes
  tiled   = smartBorders $ Tall nmaster delta ratio
  -- tiled   = smartBorders $ gaps $ Tall nmaster delta ratio

  -- The default number of windows in the master pane
  nmaster = 1

  -- Default proportion of screen occupied by master pane
  ratio   = 1 / 2

  -- Percent of screen to increment by when resizing panes
  delta   = 3 / 100

  gaps = spacingRaw False (Border 4 4 4 4) True (Border 4 4 4 4) True
  -- gaps = id


-- | Removes borders from fullscreen floating windows
data AllFloats = AllFloats deriving (Read, Show)

instance SetsAmbiguous AllFloats where
    hiddens _ wset _ _ _ = M.keys $ floating wset

-- Hooks ----------------------------------------------------------------------

myManageHook :: ManageHook
myManageHook = mconcat
  [
  -- Make popups appear as small floating windows
   stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog"
      --> doRectFloat smallRect
  , stringProperty "WM_WINDOW_ROLE" =? "gimp-layer-new"
      --> doRectFloat smallRect
  , stringProperty "WM_WINDOW_ROLE" =? "gimp-toolbox-color-dialog"
      --> doRectFloat tinyRect
  , className =? "Gnome-calculator" --> doRectFloat tinyRect

  -- Move stuff to dedicated workspaces
  , className =? "TelegramDesktop" --> doShift "(messaging)"
  , className =? "discord"         --> doShift "(messaging)"
  , className =? "Slack"           --> doShift "(messaging)"

  -- Praat popups
  , stringProperty "WM_NAME" =? "Praat Info" --> doRectFloat tinyRect

  -- Android emulator should float
  , stringProperty "_NET_WM_NAME" =? "Emulator" --> doFloat

  -- Make NO$GBA debugger behave
  , ("No$gba Emulator" `isPrefixOf`) <$> stringProperty "WM_NAME"
      --> doAbsRectFloat gbaW gbaH

  -- Misc
  , manageDocks
  , scratchpadManageHookDefault
  ]
  where
    smallRect = SS.RationalRect 0.2 0.1 0.6 0.8
    tinyRect  = SS.RationalRect 0.3 0.2 0.4 0.6
    -- Scaled up GBA screen size. + 4 to account for border
    (gbaW, gbaH) = (240 * 3 + 4, 160 * 3 + 4)


-- Set up window swallowing
mySwallowHook :: Event -> X All
mySwallowHook = swallowEventHook (className =? "kitty")
                                 ( not <$> className =? "kitty" <&&>
                                  (not <$> className =? "Polybar"))

-- myHandleEventHook :: Event -> X All
-- myHandleEventHook = mconcat
--   [ mySwallowHook
--   , dynamicPropertyChange "WM_NAME" (title =? "Spotify" --> doShift "(music)")
--   ]

myHandleEventHook :: Event -> X All
myHandleEventHook = mconcat
  [ dynamicPropertyChange "WM_NAME" (title =? "Spotify" --> doShift "(music)") ]


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
