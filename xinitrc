## .xinitrc  -- X init script
## Copyright 2008-2011 by Michal Nazarewicz (mina86@mina86.com)

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

	run() {
		if has "$1"; then
			"$@"
		fi
	}

	xset -b -c r rate 250 50 s off +dpms

	if has xcompmgr; then
		xcompmgr &
	fi

	if [ -e ~/.synergy.conf ]; then
		run synergys
	else
		run synergyc
	fi


	if [ -e /etc/default/keyboard ]; then
		. /etc/default/keyboard
		if [ -n "$XKBLAYOUT$XKBVARIANT$XKBOPTIONS" ]; then
			# xdotool works better if layout is set by setxkbmap
			setxkbmap "$XKBLAYOUT" "$XKBVARIANT" "$XKBOPTIONS"
		fi
	fi
) &

if has gpg-agent; then
	exec gpg-agent --daemon sawfish
else
	exec sawfish
fi
