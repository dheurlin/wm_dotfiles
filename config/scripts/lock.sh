#!/bin/bash

# Import pywal colors
source ~/.cache/wal/colors.sh

# Converts a color from pywal to rrggbbaa
conCol() { echo  "${1:1}FF"; }

dateStr="$(date '+%A') den $(date '+%d %B %Y')"

insideCirc=00000020
ringCol=ffffff80

# textCol=ffffffff
textCol=$(conCol $foreground)
textSize=80

wrongCol=C70039
verCol=${color2:1}
# inpCol=${color1:1}
inpCol=00C72B

main() {
    # Kill any previous instances of i3lock, to avoid the "wrong password" thing when we open the lid
    pkill i3lock
    ## When locked, the screen should go to sleep after one minute.
    ## Otherwise, it should go to sleep according to my script
    xset dpms 10
    echo "$args" | xargs i3lock
    ~/.config/scripts/sleepScreenSetup.sh
}

## A way to define the arguments which allows us to comment stuff out.
argstr=$(cat <<END
    -c $background
    -t
    -n
    --force-clock
    --ignore-empty-password
    --redraw-thread

    --radius 450
    --ring-width 60

    --timecolor=$textCol
    --datecolor=$textCol
    --date-font="SauceCodePro Nerd Font:bold"
    --time-font="SauceCodePro Nerd Font"

    --timesize=100
    --datesize=30
    --datestr="$dateStr"
    --timepos="ix:iy+30"
    --datepos="tx:ty+65"
    --indpos="x+75:y+h-175" #
    --time-align=1          # Remove these to put text in center
    --date-align=1          #

    --verifsize=$textSize
    --wrongsize=$textSize
    --verifcolor=ffffff00
    --wrongcolor=00000000

    --insidevercolor="${verCol}44"
    --insidewrongcolor="${wrongCol}44"

    # --linecolor=00000000
    # --ringcolor=00000000
    # --ringwrongcolor=00000000
    # --ringvercolor=00000000      # These guys were used for the ring setup
    # --ringcolor=00000000

    --insidecolor=$insideCirc
    --keyhlcolor="${inpCol}ff"
    --bshlcolor="${wrongCol}ff"
    --separatorcolor=00000000

    --bar-indicator
    --bar-step=100
    --bar-periodic-step=100
    --bar-max-height=32
	--bar-base-width=32 # actually the height of the base line
	--bar-width=300
    --bar-color=00000044
    --ringvercolor="${verCol}cc"
    --ringwrongcolor="${wrongCol}cc"
    # -i /home/danielheurlin/Bilder/wallpapers/minimalist-bridge-between-mountains-1920×1080.jpg
END
)

# This removes comments
args=$(echo "$argstr" | sed -e 's/#\s.*//g')


main
