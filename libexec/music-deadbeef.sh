#!/bin/sh

deadbeef_query() {
	deadbeef --nowplaying-tf '%isplaying%%ispaused%' 2>/dev/null | grep -q 1
}

_deadbeef_path() {
	deadbeef --nowplaying-tf %path%
}
_deadbeef_is_playing() {
	deadbeef --nowplaying-tf '%isplaying%' 2>/dev/null | grep -q 1
}

deadbeef_ctl() {
	case ${1:-show} in
	is-playing)
		_deadbeef_is_playing
		;;
	play|pause|stop|prev|next)
		deadbeef "--$1"
		;;
	pause-maybe)
		_deadbeef_is_playing || return 1
		deadbeef --pause
		;;
	toggle)
		deadbeef --play-pause
		;;
	show)
		deadbeef --nowplaying-tf "
\$if(%title%,
\$if(%album%,[%artist% ]«%album%» ,)[%track number%. ]%title%,
%file%)
  \[%playback_time%/%length%\]
[  (%codec%[ %codec_profile%][ %bitrate%])]
"
		;;
	file|path)
		deadbeef --nowplaying-tf %path% && echo
		;;
	dir)
		# shellcheck disable=SC2016
		deadbeef --nowplaying-tf '$directory_path(%path%)' && echo
		;;
	tag)
		file=$(deadbeef --nowplaying-tf %path%)
		if [ -z "$file" ]; then
			die "nothing playing or could not connect" >&2
		fi
		chmod 755 -- "$file"
		deadbeef --next
		;;
	*)
		die "$1: unknown or unsupported action" >&2
		return 1
	esac
}
