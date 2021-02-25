#!/bin/bash


# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch the main bar on every monitor
monitors=$(xrandr --query | grep " connected")
while read -r m; do
  polybar main --reload &
  echo "Bar launched on $monitor"
  # lower the bar so it's below fullscreen windows (20 times arbitrarily lol)
  for i in `seq 1 50`; do
    xdo lower -n tray
    xdo lower -n polybar
  done

done <<< "$monitors"
