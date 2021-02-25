{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE FlexibleContexts  #-}

import           Vars
import           Util                           ( doAbsRectFloat )
import           Bindings                       ( myBindings )
import qualified Colors                        as Col

import           XMonad                  hiding ( (|||) )
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.Named
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageHelpers
import qualified XMonad.StackSet               as SS
import           XMonad.StackSet                ( RationalRect(..)
                                                , floating
                                                , screenDetail
                                                )
import           XMonad.Util.EZConfig
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Grid
import           XMonad.Layout.LayoutModifier
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Util.SpawnOnce
import           XMonad.Util.Run
import           XMonad.Util.Scratchpad ( scratchpadManageHookDefault )
import           XMonad.Hooks.DynamicProperty
import           XMonad.Layout.Spacing

import           Text.Printf
import           Control.Monad
import           Data.Maybe
import           Data.Char
import           Data.List
import           Data.Semigroup
import           Data.Functor                   ( ($>) )
import qualified Data.Map                      as M
import qualified Codec.Binary.UTF8.String as UTF8

main = do
  safeSpawn "mkfifo" ["/tmp/.xmonad-layout-log"]
  xmonad $ ewmh
    ( desktopConfig
        { terminal           = myTerminal
        , modMask            = mod4Mask
        , workspaces         = map show [1..10] <> ["(messaging)", "(music)"]
        , layoutHook         = avoidStruts myLayout
        , borderWidth        = 2
        , normalBorderColor  = Col.unFocusedBorder
        , focusedBorderColor = Col.focusedBorder
        , handleEventHook    = docksEventHook <> handleEventHook desktopConfig <> myHandleEventHook
        , manageHook         = myManageHook
        , logHook            = eventLogHookForPolybar
        , startupHook        = startupHook desktopConfig
                               >> addEWMHFullscreen
                               >> myStartupItems
        }
      `additionalKeysP` myBindings
    )

-- Startup items --------------------------------------------------------------

myStartupItems :: X ()
myStartupItems = sequence_ [  ]


-- Bar setup     ---------------------------------------------------------------

-- this creates a FIFO containing the current layout which polybar can read from
eventLogHookForPolybar :: X ()
eventLogHookForPolybar = do
  winset <- gets windowset
  let ld = description . SS.layout . SS.workspace . SS.current $ winset
  io $ appendFile "/tmp/.xmonad-layout-log" (ld ++ "\n")


-- Layouts --------------------------------------------------------------------
myLayout = lessBorders AllFloats $
              (named "Tall"         $ gaps tiled)
          ||| (named "Mirror tall"  $ gaps $ Mirror tiled)
          ||| (named "Grid"         $ gaps Grid)
          ||| (named "Full"         $ noBorders Full)
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

  gaps = spacingRaw False (Border 5 5 5 5) True (Border 5 5 5 5) True
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

  --- No gaps for browser windows TODO not working!
  , className =? "Nightly" --> liftX (setSmartSpacing False) $> Endo id

  -- Move stuff to dedicated workspaces
  , className =? "TelegramDesktop" --> doShift "(messaging)"
  , className =? "discord"         --> doShift "(messaging)"

  -- Make NO$GBA debugger behave
  , ("No$gba Emulator" `isPrefixOf`) <$> stringProperty "WM_NAME"
      --> doAbsRectFloat gbaW gbaH

  -- Misc
  , manageDocks
  , scratchpadManageHookDefault
  ]
  where
    smallRect = RationalRect 0.2 0.1 0.6 0.8
    tinyRect  = RationalRect 0.3 0.2 0.4 0.6
    -- Scaled up GBA screen size. + 4 to account for border
    (gbaW, gbaH) = (240 * 3 + 4, 160 * 3 + 4)


myHandleEventHook :: Event -> X All
myHandleEventHook = mconcat
  [ fullscreenEventHook
  , dynamicPropertyChange "WM_NAME" (title =? "Spotify" --> doShift "(music)")
  ]


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
