#! /bin/sh

maxlen=$1

trim() {
  if [ "$(echo "$1" | wc -m)" -ge "$maxlen" ]; then
    echo "$(echo "$1" | cut -c -${maxlen})..."
  else
    echo "$1"
  fi
}

status=$(playerctl status 2> /dev/null)
case $status in
  "Playing")
    artist=$(playerctl metadata --format "{{artist}}")
    title=$(playerctl metadata --format "{{title}}")
    echo "$(trim "$artist"): $(trim "$title")"
    ;;
  *)
    echo "-"
    ;;
esac
