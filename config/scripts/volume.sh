#! /bin/sh

# Argument should be e.g. +5% or -5%

# volSound=/home/danielheurlin/.config/scripts/sounds/bark.ogg
volSound=/home/danielheurlin/.config/scripts/sounds/bark.wav
play_sound() { aplay $volSound; }

pactl set-sink-mute @DEFAULT_SINK@ 0
pactl set-sink-volume @DEFAULT_SINK@ $1
# $(pacmd list-sink-inputs | grep -c 'state: RUNNING') || play_sound

if ! pacmd list-sink-inputs | grep -c 'state: RUNNING'; then
    play_sound
fi
