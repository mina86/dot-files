#!/bin/sh

_spotify() {
	dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify \
		/org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.$1 \
		>/dev/null
}

spotify_query() {
	qdbus org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 \
		org.freedesktop.DBus.Properties.Get \
		org.mpris.MediaPlayer2.Player PlaybackStatus
}

spotify_ctl() {
	case ${1:-show} in
	play)
		_spotify Play
		;;
	pause)
		_spotify Pause
		;;
	toggle)
		_spotify PlayPause
		;;
	next)
		_spotify Next
		;;
	prev)
		_spotify Previous
		;;
	stop)
		_spotify Stop
		;;
	is-playing)
		spotify_query | grep -wq Playing
		;;
	pause-maybe)
		if spotify_query | grep -wq Playing; then
			_spotify Pause
		else
			return 1
		fi
		;;
	*)
		echo "${0##*/}: $1: unknown or unsupported action" >&2
		return 1
	esac
}
