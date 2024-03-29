#!/bin/bash
set -e

# Volume notification: Pulseaudio and dunst
# inspired by gist.github.com/sebastiencs/5d7227f388d93374cebdf72e783fbd6a

icon_path=/usr/share/icons/Adwaita/64x64/status/
sink_nr=0   # use `pactl list-sinks` to find out sink_nr

function get_volume {
    pactl list-sinks | awk '/\tvolume:/ { print $5 }' | tail -n+$((sink_nr+1)) | head -n1 | cut -d '%' -f 1
}

function get_volume_icon {
    if [ "$1" -lt 34 ]
    then
        echo -n "audio-volume-low-symbolic.symbolic.png"
    elif [ "$1" -lt 67 ]
    then
        echo -n "audio-volume-medium-symbolic.symbolic.png"
    elif [ "$1" -le 100 ]
    then
        echo -n "audio-volume-high-symbolic.symbolic.png"
    else
        echo -n "audio-volume-overamplified-symbolic.symbolic.png"
    fi
}

function volume_notification {
    volume=`get_volume`
    echo "voluym!" $volume
    vol_icon=`get_volume_icon $volume`
    bar=$(seq -s "─" $(($volume / 5)) | sed 's/[0-9]//g')
    #dunstify -u low -i $icon_path$vol_icon $bar
    echo "vol"
}

function mute_notification {
    muted=$(pactl list-sinks | awk '/muted/ { print $2 }' | tail -n+$((sink_nr+1)) | head -n1)
    if [ $muted == 'yes' ]
    then
        echo "mute"
        #dunstify low -i ${icon_path}audio-volume-muted-symbolic.symbolic.png mute
    else
        echo "unmute"
        #dunstify low -i ${icon_path}`get_volume_icon $(get_volume)` unmute
    fi
}

case $1 in
    up)
        pactl set-sink-volume $sink_nr +5%
        volume_notification
        ;;
    down)
        pactl set-sink-volume $sink_nr -5%
        volume_notification
	    ;;
    mute)
        pactl set-sink-mute $sink_nr toggle
        mute_notification
        ;;
    *)
        echo "Usage: $0 up | down | mute"
        ;;
esac
