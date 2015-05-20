## .xinitrc  -- X init script
## Copyright 2008-2015 by Michal Nazarewicz (mina86@mina86.com)

cd ~

exec >>/tmp/sawfish-log 2>&1

has() {
	which "$1" >/dev/null 2>&1
}

(
	for DIR in /usr/X11R6/lib/X11/xinit/ .; do
		[ -f "$DIR/.Xresources" ]  && xrdb -merge "$DIR/.Xresources"
		[ -f "$DIR/.Xmodmap" ]     && xmodmap     "$DIR/.Xmodmap"
	done

	cd /

	if [ -f ~/.xscreen-config ]; then
		. ~/.xscreen-config
	fi

	if has xcompmgr; then
		xcompmgr &
	fi

	if [ -x ~/bin/libexec/xinit-kbd ]; then
		~/bin/libexec/xinit-kbd
	fi

	if [ -x ~/bin/redshift ] && [ -e ~/.config/redshift.conf ]; then
		~/bin/redshift &
	fi
) &

SAWFISH_QUIT_CMD_FILE=$(mktemp)
trap 'rm -f -- "$SAWFISH_QUIT_CMD_FILE"' 0
export SAWFISH_QUIT_CMD_FILE

if has gpg-agent; then
	gpg-agent --daemon sawfish
else
	sawfish
fi

if [ -s "$SAWFISH_QUIT_CMD_FILE" ] && read cmd <$SAWFISH_QUIT_CMD_FILE; then
	rm -f -- "$SAWFISH_QUIT_CMD_FILE"
	case "$cmd" in
	poweroff|reboot)
		exec /sbin/$cmd
		;;
	reboot-w)
		exec /usr/local/sbin/reboot-w
	esac
fi
