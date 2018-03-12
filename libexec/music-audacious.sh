#!/bin/sh

audacious_query() {
	_=$(audtool --current-song) && test "$_" != "No song playing."
}

audacious_ctl() {
	case $1 in
	play|pause|stop)
		audacious --$1
		;;
	toggle)
		audacious --play-pause
		;;
	next)
		audacious --fwd
		;;
	prev)
		audacious --rew
		;;
	show)
		audtool --current-song
		;;
	is-playing)
		audtool --playback-playing
		;;
	pause-maybe)
		if audtool --playback-playing; then
			audacious --pause
		else
			return 1
		fi
		;;
	*)
		echo "${0##*/}: $1: unknown or unsupported action" >&2
		return 1
	esac
}
