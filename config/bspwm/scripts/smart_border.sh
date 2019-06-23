#! /bin/bash

#########################################################
# Removes borders if there is only one non-floating
# window on a desktop
#########################################################

border_width=`bspc config border_width`

# Gets the desktops we need to check
get_desktops() {
  event="$1"
  event_type=`echo $event | cut -f 1 -d " "`
  case $event_type in
    "node_add")
        desktops=`echo "$event" | awk '{print $3}'`
     ;;
    "node_remove")
        desktops=`echo "$event" | awk '{print $3}'`
     ;;
    "node_transfer")
        desktops=`echo "$event" | awk '{print $3 " " $6}'`
     ;;
  esac
  echo "$desktops"
}

# Given a list of desktops, fixes the borders on each of them
handle_desktops() {
  desktops="$1"
  for desktop in $desktops; do

    name=` bspc query -D --desktop $desktop --names`
    nodes=`bspc query -N --desktop $desktop -n .!floating`

    if [ `echo "$nodes" | wc -l` -eq 1 ]; then
      echo "Removing borders on desktop $name"
      bspc config -n $nodes border_width 0
    else
      echo "Replacing borders on desktop $name"
      bspc config -d $desktop border_width $border_width
    fi
  done
}

# Fixes the borders on every desktop
handle_all_desktops() {
  handle_desktops "$(bspc query -D)"
}

# Main loop: when a node event occurs, fix the relevant desktops
main() {
  bspc subscribe node_add node_remove node_transfer | while read -r event; do
    desktops=`get_desktops "$event"`
    handle_desktops "$desktops"
  done
}

# We can also receive SIGUSR1 to force fixing all desktops
trap "handle_all_desktops; main" SIGUSR1

main

