#! /bin/sh

# Argument should be e.g. +5% or -5%

# volSound=/home/danielheurlin/.config/scripts/sounds/bark.ogg
volSound=/home/danielheurlin/.config/scripts/sounds/bark.wav
play_sound() { aplay $volSound; }

amixer set Master $1
amixer set Master unmute

if ! pacmd list-sink-inputs | grep -c 'state: RUNNING'; then
    echo "playing"
    play_sound
fi
