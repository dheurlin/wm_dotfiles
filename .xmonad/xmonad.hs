{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE FlexibleContexts  #-}

import           Util
import           Vars
import           Bindings                       ( myBindings )
import qualified Colors                        as Col

import           XMonad
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageHelpers
import           XMonad.StackSet                ( RationalRect(..)
                                                , floating
                                                )
import           XMonad.Util.EZConfig
import           XMonad.Layout.NoBorders
import           XMonad.Layout.LayoutModifier
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Util.SpawnOnce
import           XMonad.Util.Run
import           XMonad.Hooks.DynamicProperty

import           Control.Monad
import           Data.Maybe
import           Data.Char
import           Data.Semigroup
import qualified Data.Map                      as M
import           Data.Functor
import qualified Codec.Binary.UTF8.String as UTF8

main = xmonad =<< myXmobar
  (                 desktopConfig
      { terminal           = myTerminal
      , modMask            = mod4Mask
      , workspaces         = map show [1..10] <> ["(messaging)", "(music)"]
      , layoutHook         = myLayout
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
myStartupItems = sequence_ [ spawnTrayer ]

spawnTrayer :: X ()
spawnTrayer = do
  spawnOnce $ "trayer " <> trayeropts
  -- spawn $ "trayer " <> trayeropts
  -- Put trayer below fullscreen windows
  spawn "xdo above -t \"$(xdo id -n xmobar)\" \"$(xdo id -N trayer -m)\""
 where
   trayeropts = unwords [ "--widthtype"    , "request"
                        , "--height"       , "14"
                        , "--edge"         , "top"
                        , "--align"        , "left"
                        , "--distancefrom" , "left"
                        , "--distance"     , "5"
                        , "--transparent"  , "true"
                        , "--alpha"        , "0"
                        , "--tint"         , show $ "0x" <> tail Col.bg
                        , "--iconspacing"  , "5"
                        ]

-- | Gets the current with of the system tray in pixels
getTrayWidth :: IO Int
getTrayWidth =
  readProcess "xdo" ["id", "-N", "trayer"]
    <&> (["getwindowgeometry"] <>) . (: [])
    >>= readProcess "xdotool"
    <&> lines
    <&> filter ((== "  Geometry") . take 10)
    <&> read . takeWhile isNumber . dropWhile (not . isNumber) . head

-- | Gets the current with of the system tray in characters of the bar font
trayWidthChars :: IO Int
trayWidthChars = getTrayWidth <&> (// (fontWidth - 3))

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
  opts = unwords ["-F", "gray", "-B", show Col.bg, "-f", show $ "xft:" <> font]
  modifyOutput s = do
    numSpaces <- trayWidthChars
    pure $ replicate (numSpaces + 1) ' ' <> "| " <> s

myXmobarPP = xmobarPP
  { ppCurrent = xmobarColor "white" Col.accentBg . wrap " " " " . ppWorkspace

   -- only print non-numeric empty ws
  , ppHiddenNoWindows = \case
      str | all isNumber str -> ""
          | otherwise        -> ppWorkspace str

  , ppVisible = ppVisible xmobarPP . ppWorkspace
  , ppHidden  = ppHidden  xmobarPP . ppWorkspace
  , ppUrgent  = ppUrgent  xmobarPP . ppWorkspace
  }

-- | Formatting for special workspaces
ppWorkspace :: String -> String
ppWorkspace "(music)"     = "<icon=music.xbm/>"
ppWorkspace "(messaging)" = "<icon=mail.xbm/>"
ppWorkspace s             = s



-- Layouts --------------------------------------------------------------------
myLayout = lessBorders AllFloats $ tiled ||| Mirror tiled ||| noBorders Full
 where
   -- default tiling algorithm partitions the screen into two panes
  tiled   = smartBorders $ Tall nmaster delta ratio

  -- The default number of windows in the master pane
  nmaster = 1

  -- Default proportion of screen occupied by master pane
  ratio   = 1 / 2

  -- Percent of screen to increment by when resizing panes
  delta   = 3 / 100

-- | Removes borders from fullscreen floating windows
data AllFloats = AllFloats deriving (Read, Show)

instance SetsAmbiguous AllFloats where
    hiddens _ wset _ _ _ = M.keys $ floating wset

-- Hooks ----------------------------------------------------------------------
myManageHook :: ManageHook
myManageHook = mconcat
  [ stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog"
      --> doRectFloat (RationalRect 0.2 0.1 0.6 0.8)
  , className =? "TelegramDesktop" --> doShift "(messaging)"
  , className =? "discord"         --> doShift "(messaging)"
  , manageDocks
  ]

-- doRectFloat

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
