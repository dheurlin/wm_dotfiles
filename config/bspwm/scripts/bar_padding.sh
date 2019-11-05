#! /bin/bash

######################################
# Sets the top padding on each monitor
# to compensate for the height of the
# status bar
######################################

set_all_padding() {
  # Find each polybar instance and figure out its height and what monitor it's on
  xdotool search --classname polybar | while read -r bar_id; do
    info=`xwininfo -id $bar_id`
    monitor=`echo "$info" | grep xwininfo | awk '{print $5}' | sed 's/.*_\(.*\)"/\1/g'`
    height=` echo "$info" | grep Height   | awk '{print $2}'`
    set_padding_for_monitor $monitor $height
  done
}

set_padding_for_monitor() {
  # Adjusts the padding for a given monitor for a bar of the given height.
  # NOTE: This assumes that we want the top padding as bottom padding
  monitor=$1
  height=$2
  let totalheight=`bspc config -m $monitor bottom_padding`+$height
  bspc config -m $monitor top_padding $totalheight
}

while true; do
  set_all_padding
  sleep 5
done
