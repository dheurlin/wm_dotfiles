#! /bin/sh

## Starts our notification server of choice,
# currently dunst

source "${HOME}/.cache/wal/colors.sh"

dunst \
    \ # -lb "${color0:-#F0F0F0}" \
    \ # -nb "${color0:-#F0F0F0}" \
    \ # -cb "${color0:-#F0F0F0}" \
    -lb "#000000" \
    -nb "#000000" \
    -cb "#000000" \
    -lf "${color15:=#000000}" \
    -bf "${color15:=#000000}" \
    -cf "${color15:=#000000}" \
    -nf "${color15:=#000000}"
