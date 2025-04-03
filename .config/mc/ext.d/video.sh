#!/bin/sh

action=$1
filetype=$2

do_open_action() {
    filetype=$1

    case "${filetype}" in
    *)
        mpv "${MC_EXT_FILENAME}" 2>&1
        osascript -e 'tell application "iTerm" to activate'
        ;;
    esac
}

case "${action}" in
view)
    echo "Viewing video files is not supported"
    ;;
open)
    do_open_action "${filetype}"
    ;;
*)
    ;;
esac
