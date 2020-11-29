#! /bin/sh

choice=$(printf "shut down\nrestart" | dmenu -p "What to do?: ")

confirm() {
    choice=$(printf "no\nyes" | dmenu -p "Really $1 ? ")
    case $choice in
        yes)
            printf 1
            ;;
        no)
            printf 0
            ;;
        esac
}

case $choice in
    "shut down")
        confirm=$(confirm $choice)
        [ $confirm -ne 0 ] && shutdown now
        ;;
    "restart")
        confirm=$(confirm $choice)
        [ $confirm -ne 0 ] && reboot
        ;;
esac
