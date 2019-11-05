#! /bin/bash
gaps_file="$HOME/wm_dotfiles/config/bspwm/scripts/.gaps"

if [ -f "$gaps_file" ]; then
    # read variables from file

    readarray idiots < "$gaps_file"

    # bspc config -d focused window_gap      "${idiots[0]}"
    # bspc config -d focused top_padding     "${idiots[1]}"
    # bspc config -d focused bottom_padding  "${idiots[2]}"
    # bspc config -d focused left_padding    "${idiots[3]}"
    # bspc config -d focused right_padding   "${idiots[4]}"

    bspc config -d focused window_gap      0
    bspc config -d focused top_padding     0
    bspc config -d focused bottom_padding  0
    bspc config -d focused left_padding    0
    bspc config -d focused right_padding   0

    rm "$gaps_file"

else
    touch "$gaps_file"
    # Save current gaps and padding to file
    bspc config -m focused window_gap     >> "$gaps_file"
    bspc config -m focused top_padding    >> "$gaps_file"
    bspc config -m focused bottom_padding >> "$gaps_file"
    bspc config -m focused left_padding   >> "$gaps_file"
    bspc config -m focused right_padding  >> "$gaps_file"

    # Set idiots to 0
    bspc config -d focused window_gap     0
    bspc config -d focused top_padding    "-`bspc config -m focused top_padding`"
    bspc config -d focused bottom_padding "-`bspc config -m focused bottom_padding`"
    bspc config -d focused left_padding   "-`bspc config -m focused left_padding`"
    bspc config -d focused right_padding  "-`bspc config -m focused right_padding`"
fi
