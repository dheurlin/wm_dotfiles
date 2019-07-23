#! /bin/sh

# Argument should be e.g. +5% or -5%

volSound=/home/danielheurlin/.config/scripts/sounds/volume.ogg
# volSound=/usr/share/sounds/gnome/default/alerts/bark.ogg

pactl set-sink-mute @DEFAULT_SINK@ 0 && \
pactl set-sink-volume @DEFAULT_SINK@ $1 && \
(pacmd list-sink-inputs | grep -c 'state: RUNNING' || play $volSound)
