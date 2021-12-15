#!/bin/bash

echo "kebab"

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch the main bar on every monitor
monitors=$(xrandr --query | grep " connected" | cut -d" " -f1)
echo "$monitors"
while read -r m; do
  MONITOR=$m polybar main --reload & > logs 2>&1
  echo "Bar launched on $m"
  # lower the bar so it's below fullscreen windows (50 times arbitrarily lol)
  for i in `seq 1 50`; do
    xdo lower -n tray
    xdo lower -n polybar
  done

done <<< "$monitors"
