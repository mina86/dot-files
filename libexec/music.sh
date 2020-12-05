#!/bin/sh

set -eu

die() {
	echo "${0##*/}: $*" >&2
	exit 1
}

if [ $# -eq 0 ]; then
	set -- show
fi

if [ -x ~/.local/libexec/"music-pre-$1.sh" ]; then
	source ~/.local/libexec/"music-pre-$1.sh" "$@"
	exit $?
fi

found=false
for player in deadbeef audacious mpd; do
	if [ -x ~/.local/libexec/music-$player.sh ]; then
		. ~/.local/libexec/music-$player.sh
		if "${player}_query" >/dev/null 2>&1; then
			found=true
			break
		fi
	fi
done

if ! $found; then
	die "unable to identify currently running music player"
fi

case ${1-} in if)
	if [ $# -eq 1 ]; then
		die "‘if’ command requires an argument"
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

${player}_ctl "$@"
