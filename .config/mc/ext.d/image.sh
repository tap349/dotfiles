#!/bin/sh

# $1 - action
# $2 - type of file

action=$1
filetype=$2

[ -n "${MC_XDG_OPEN}" ] || MC_XDG_OPEN="xdg-open"

do_view_action() {
    filetype=$1

    case "${filetype}" in
    *)
        identify "${MC_EXT_FILENAME}"
        which exif >/dev/null 2>&1 && exif "${MC_EXT_FILENAME}" 2>/dev/null
        ;;
    esac
}

do_open_action() {
    filetype=$1

    case "${filetype}" in
    *)
        # See mvi alias (zsh aliases are not visible here)
        (mpv --keep-open-pause=yes "${MC_EXT_FILENAME}" >/dev/null 2>&1 &)
        ;;
    esac
}

case "${action}" in
view)
    do_view_action "${filetype}"
    ;;
open)
    ("${MC_XDG_OPEN}" "${MC_EXT_FILENAME}" >/dev/null 2>&1) || \
        do_open_action "${filetype}"
    ;;
*)
    ;;
esac
