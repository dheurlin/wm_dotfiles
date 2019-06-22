#!/bin/zsh

# Parameters for different DPIs
typeset -A params

params[4_height]=25
params[4_f0]="SourceCodePro:size=9;1"
params[4_f1]="IcoFont:size=11;2"
params[4_f2]="WifiIcons:size=11;3"

params[7_height]=32
params[7_f0]="SourceCodePro:size=11;2"
params[7_f1]="IcoFont:size=13;3"
params[7_f2]="WifiIcons:size=13;5"

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch the bar
launchbar() { polybar main --reload & }

# Takes a line rendered by `xrandr --query | grep " connected`" and
# returns the dpi for that monitor (in px per mm)

ceil() {
  echo `bc -l <<< "$1 + 0.999" | grep -o '[^\.]*' | head -1`
}

getdpi() {
  local widthmm=`echo $1 | grep -Eo '[0-9]+mm' | head -1 | sed s/mm//g`
  local widthpx=`echo $1 | grep -Eo '[0-9]+x' | sed s/x//g`
  bc <<< "scale=2;$widthpx/$widthmm"
}

# Launch the main bar on every monitor
monitors=$(xrandr --query | grep " connected")
while read -r m; do

  echo "############################### $m"

  monitor=$(echo $m | cut -d" " -f1)

  dpi=$(getdpi $m)
  dpi=`ceil $(getdpi $m)`
  echo "[info] DPI of $monitor: $dpi"

  MONITOR=$monitor              \
  HEIGHT=$params[${dpi}_height] \
  F0=$params[${dpi}_f0]         \
  F1=$params[${dpi}_f1]         \
  F2=$params[${dpi}_f2]         \
  launchbar

  echo "Bar launched on $monitor"


done <<< "$monitors"

# hide the system tray and save the new PID in a file
# hideIt.sh --name '^Polybar tray window$' --signal --wait --peek 0 &>/dev/null &
# echo $! > ~/.config/polybar/.hideit

