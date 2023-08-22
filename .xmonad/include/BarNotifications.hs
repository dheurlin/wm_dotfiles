module BarNotifications where
import XMonad (spawn, MonadIO)

showVolumeNotification :: MonadIO m => m ()
showVolumeNotification = spawn $ unwords
  [ "notify-send"
  , show "Volume"
  , "-h" , "int:value:$(pulseaudio-control --format '$VOL_LEVEL' output)"
  , "-h",  "string:synchronous:volume"
  , "-t" , show 5000
  ]
