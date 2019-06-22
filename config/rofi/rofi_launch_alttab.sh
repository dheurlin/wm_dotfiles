#! /bin/sh

rofi \
    -fullscreen \
    -show window  \
    -kb-cancel "Alt+Escape,Escape" \
    -kb-accept-entry "!Alt+Alt_L,Return"\
    -kb-row-down "Alt+Tab,Alt+Down,Alt+j" \
    -kb-row-up "Alt+ISO_Left_Tab,Alt+Up,Alt+k"&
