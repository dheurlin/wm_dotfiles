#!/bin/sh

# Set mouse acceleration
xinput --set-prop 'DLL082A:01 06CB:76AF Touchpad' 'Device Accel Profile' 3
xset m 15/10 0

## Remap the keyboard in the following way:
# Esc = CapsLock
# CapsLock + any key = AltGr + any key
# CapsLock alone = Escape
$HOME/.config/scripts/kbd-setup.sh

# Disable touchpad while typing
# dispad # OLD VERSION
$HOME/scripts/disable_trackpad_while_typing.py &

# enable key rebinding
xbindkeys

# sleep 1
# amixer -c PCH cset 'name=Headphone Mic Boost Volume' 1
