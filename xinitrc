
## .xinitrc  -- X init script
## Copyright 2008-2011 by Michal Nazarewicz (mina86@mina86.com)

cd ~
for DIR in /usr/X11R6/lib/X11/xinit/ .; do
	[ -f "$DIR/.Xresources" ]  && xrdb -merge "$DIR/.Xresources"
	[ -f "$DIR/.Xmodmap" ]     && xmodmap     "$DIR/.Xmodmap"
done

has() {
	which "$1" >/dev/null 2>&1
}

(
	sleep 2

	cd /

	run() {
		if has "$1"; then
			"$@"
			return 0
		fi
		return 1
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

	if [ -f ~/.wallpaper.bmp ]; then
		run Esetroot -scale ~/.wallpaper.bmp &
	fi

	run xscreensaver -no-splash
) >/dev/null 2>&1 </dev/null &

if has gpg-agent; then
	exec gpg-agent --daemon sawfish
else
	exec sawfish
fi
