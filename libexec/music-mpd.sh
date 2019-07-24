#!/bin/sh

_mpc() {
	exec 3>&1
	return $(
		exec 4>&1 >&3
		{
			mpc --format '[[[%artist% <&%album%> ]|[%artist% - ]|[<%album%> ]]%title%]|[%file%]' "$@" || echo $? >&4
		} | sed -ne '
		1 h
		/^\(\[[a-z]*\] \).*/ {
			s//\1/
			G
			s/\n//
			x
		}
		$ {
			x
			p
		}
	')
}

_mpc_file() {
	mpc --format %file% | head -n1
}

mpd_file() {
	file=${MPD_MUSIC_DIR:-/o/music}/$(_mpc_file)
}

mpd_query() {
	which mpc >/dev/null 2>&1
}

mpd_ctl() {
	case ${1:-show} in
	play|pause|stop|prev|toggle|next)
		_mpc "$@"
		;;
	rewind)
		_mpc seek 0:00:00
		;;
	is-playing)
		mpc -f '' | grep -wq playing
		;;
	file|path)
		mpd_file
		echo "$file"
		;;
	dir)
		mpd_file
		dirname "$file"
		;;
	show)
		mpc
		;;
	pause-maybe)
		mpc pause-if-playing
		;;
	tag)
		file=$(_mpc_file)
		if [ -z "$file" ]; then
			echo "${0##*/}: nothing playing or could not connect" >&2
			exit 1
		fi
		mpc sticker "$file" set tag yes
		_mpc next
		;;
	*)
		mpc "$@"
	esac
}
