#!/bin/sh
pkill xcape



# Set key repeat speed
xset r rate 200 30

# Start by swapping capslock and escape
setxkbmap -option
# setxkbmap -option -option caps:swapescape
setxkbmap -layout se,us,gr,ru -option -option caps:escape
# Temporarily map Escape to a random keycode, since xcape
#    requires the key it's mapping to be assigned to someting
xmodmap -e "keycode 255 = Escape"
# Now, map CapsLock to AltGr instead of esc
xmodmap -e "keycode 66 = ISO_Level3_Shift"
# Finally, use xcape to make AltGr = Escape
#   if tapped without any other key
# pkill xcape
xcape -e "ISO_Level3_Shift=Escape"
