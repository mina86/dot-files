#!/bin/sh

set -eu

if [ -x ~/bin/libexec/"music-pre-$1.sh" ]; then
	source ~/bin/libexec/"music-pre-$1.sh"
	exit $?
fi

found=false
for player in audacious spotify mpd; do
	if [ -x ~/bin/libexec/music-$player.sh ]; then
		source ~/bin/libexec/music-$player.sh
		if "${player}_query" >/dev/null 2>&1; then
			found=true
			break
		fi
	fi
done

if ! $found; then
	echo "${0##*/}: unable to identify currently running music player" >&2
	exit 1
fi

case ${1-} in if)
	if [ $# -eq 1 ]; then
		echo "${0##*/}: ‘if’ command requires an argument" >&2
		exit 1
	elif [ x"$2" != x"$player" ]; then
		echo "${0##*/}: running $player, not $2" >&2
		exit 255
	elif [ $# -eq 2 ]; then
		exit 0
	else
		shift 2
	fi
esac

if [ $# -eq 0 ]; then
	set -- show
fi

${player}_ctl "$@"
