#! /bin/bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch the main bar on every monitor
monitors=$(xrandr --query | grep " connected" | cut -d" " -f1)
echo "$monitors"
while read -r m; do
  echo "Hello, this is $m"
  # MONITOR=$m polybar main --reload & > logs 2>&1
  MONITOR=$m polybar main --reload &
  echo "Bar launched on $m"
done <<< "$monitors"
