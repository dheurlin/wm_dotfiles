#! /bin/sh

if [ -z "$(hyprshade current)" ]; then
  echo '{ "text": "", "alt": "", "tooltip": "", "percentage": "", "class": "inactive" }'
else
  echo '{ "text": "", "alt": "", "tooltip": "", "percentage": "", "class": "active" }'
fi
