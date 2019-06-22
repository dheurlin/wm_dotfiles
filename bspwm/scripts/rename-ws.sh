#! /bin/bash

# Renames all numeric workspaces so that they are in order,
# retaining the names of all non-numeric ones

# Give them all temporary names so we can swap names freely
re='^[0-9]+$'
bspc query -D --names | while read -r name; do
  [[ $name =~ $re ]] && bspc desktop $name -n "temp$name"
done

# Rename them into their final names
counter=1
bspc query -D --names | while read -r name; do
  [[ $name =~ 'temp' ]] && bspc desktop $name -n $((counter++))
done
