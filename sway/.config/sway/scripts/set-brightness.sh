#!/bin/bash

# You can call this script like this:
# $./brightness.sh up 5
# $./brightness.sh down 5

# icon
brightnessicon=/usr/share/icons/Papirus-Dark/symbolic/status/display-brightness-symbolic.svg

function get_brightness {
    light | grep -oE '^[0-9]+'
}

function send_notification {
    brightness=`get_brightness`
   # Send the notification
    dunstify -i $brightnessicon -t 1600 -h string:x-dunst-stack-tag:brightness -u normal "Brightness $brightness%" -h int:value:"$brightness"
}

case $1 in
    up)
	light -A $2 > /dev/null
	send_notification
	;;
    down)
	light -U $2 > /dev/null
	send_notification
	;;
esac
