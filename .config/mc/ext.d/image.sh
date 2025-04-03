#!/bin/sh

action=$1
filetype=$2

do_view_action() {
    filetype=$1

    case "${filetype}" in
    *)
        identify "${MC_EXT_FILENAME}"
        ;;
    esac
}

do_open_action() {
    filetype=$1

    case "${filetype}" in
    *)
        # See mvi alias (zsh aliases are not visible here)
        mpv --force-window=immediate --image-display-duration=inf "${MC_EXT_FILENAME}" >/dev/null 2>&1
        osascript -e 'tell application "iTerm" to activate'
        ;;
    esac
}

case "${action}" in
view)
    do_view_action "${filetype}"
    ;;
open)
    do_open_action "${filetype}"
    ;;
*)
    ;;
esac
