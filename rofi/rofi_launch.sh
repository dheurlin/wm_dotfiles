#!/bin/zsh
# Hide mouse cursor
unclutter -grab -idle 0 &
pid=$!

zsh -c "rofi -fullscreen -show combi"

# get back cursor
kill $pid

