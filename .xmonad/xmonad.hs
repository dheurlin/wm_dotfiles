{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE FlexibleContexts  #-}

import           Vars
import           Bindings                       ( myBindings )
import qualified Colors                        as Col

import           XMonad                  hiding ( (|||) )
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.Named
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageHelpers
import           XMonad.StackSet                ( RationalRect(..)
                                                , floating
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
import           Data.Semigroup
import           Data.Functor                   ( ($>) )
import qualified Data.Map                      as M
import qualified Codec.Binary.UTF8.String as UTF8

main = xmonad =<< myXmobar
  (                 desktopConfig
      { terminal           = myTerminal
      , modMask            = mod4Mask
      , workspaces         = map show [1..10] <> ["(messaging)", "(music)"]
      , layoutHook         = myLayout
      , borderWidth        = 2
      , normalBorderColor  = Col.unFocusedBorder
      , focusedBorderColor = Col.focusedBorder
      , handleEventHook    = handleEventHook desktopConfig <> myHandleEventHook
      , manageHook         = myManageHook
      , startupHook        = startupHook desktopConfig
                             >> addEWMHFullscreen
                             >> myStartupItems
      }
  `additionalKeysP` myBindings
  )

-- Startup items --------------------------------------------------------------
myStartupItems :: X ()
myStartupItems = sequence_ [ spawnTray ]

spawnTray :: X ()
spawnTray = do
  spawn $ "stalonetray " <> trayopts
  -- spawnOnce $ "stalonetray " <> trayopts
  spawn "sleep 2 && xdo above -t \"$(xdo id -n xmobar)\" \"$(xdo id -N stalonetray -m)\""
  where
    trayopts = unwords [ "-bg"        , show Col.bg
                       , "--icon-size", "16"
                       , "--geometry" , "1x1+10+0"
                       , "--slot-size", "24"
                       ]

-- XMobar setup ---------------------------------------------------------------
myStatusBar
  :: LayoutClass l Window
  => String
  -> PP
  -> (String -> IO String)
  -> XConfig l
  -> IO (XConfig (ModifiedLayout AvoidStruts l))
myStatusBar cmd pp modifyOutput conf = do
  h <- spawnPipe cmd
  return $ docks $ conf
    { layoutHook = avoidStruts (layoutHook conf)
    , logHook    = do
        logHook conf
        dynamicLogWithPP pp
          { ppOutput = modifyOutput >=> hPutStrLn h . UTF8.decodeString }
    }

myXmobar = myStatusBar ("xmobar " <> opts) myXmobarPP modifyOutput
 where
  opts = []
  modifyOutput = pure

myXmobarPP = xmobarPP
  { ppCurrent = xmobarColor "white" Col.accentBg . wrap " " " " . fmtWorkspace

   -- only print non-numeric empty ws
  , ppHiddenNoWindows = \case
      str | all isNumber str -> ""
          | str == "NSP"     -> "" -- hide NSP
          | otherwise        -> fmtWorkspace str

  , ppHidden  = \case
      "NSP" -> "" -- hide NSP
      s     -> ppHidden xmobarPP . fmtWorkspace $ s

  , ppVisible = ppVisible xmobarPP . fmtWorkspace
  , ppUrgent  = ppUrgent  xmobarPP . fmtWorkspace
  }

-- | Final markup for workspaces
fmtWorkspace :: String -> String
fmtWorkspace s = mkClickable s (ppWorkspace s)

-- | Convert special workspaces to icons
ppWorkspace :: String -> String
ppWorkspace "(music)"     = "<icon=music.xbm/>"
ppWorkspace "(messaging)" = "<icon=mail.xbm/>"
ppWorkspace s             = s

-- | Makes clicking a workspace number shift to that workspace,
-- and scrolling the workspace area scroll through workspaces
mkClickable :: String -> String -> String
mkClickable name ss =
  printf (concat [ "<action=`xdotool key Super_L+%s` button=1>"
                 ,   "<action=`xdotool key Alt_L+j` button=4>"
                 ,      "<action=`xdotool key Alt_L+k` button=5>"
                 ,        "%s"
                 ,     "</action>"
                 ,   "</action>"
                 , "</action>"
                 ])
    (key name) ss
 where
  key "(music)"     = "m"
  key "(messaging)" = "s"
  key s             = s

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

  --- No gaps for browser windows TODO not working!
  , className =? "Nightly" --> liftX (setSmartSpacing False) $> Endo id

  -- Move stuff to dedicated workspaces
  , className =? "TelegramDesktop" --> doShift "(messaging)"
  , className =? "discord"         --> doShift "(messaging)"
  --
  , manageDocks
  , scratchpadManageHookDefault
  ]
  where
    smallRect = RationalRect 0.2 0.1 0.6 0.8
    tinyRect  = RationalRect 0.3 0.2 0.4 0.6

-- WM_WINDOW_ROLE(STRING) : gimp-toolbox-color-dialog
-- WM_WINDOW_ROLE(STRING) = "gimp-layer-new"

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
