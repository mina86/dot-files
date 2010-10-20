
## .xinitrc  -- X init script
## Copyright 2008 by Michal Nazarewicz (mina86/AT/mina86.com)

cd
for DIR in /usr/X11R6/lib/X11/xinit/ .; do
	[ -f "$DIR/.Xresources" ]  && xrdb -merge "$DIR/.Xresources"
	[ -f "$DIR/.Xmodmap" ]     && xmodmap     "$DIR/.Xmodmap"
done

(

	# Let window manager start earlier.  We can wait a bit.
	sleep 2

	xset -b -c r rate 250 50 s off +dpms

	if which xcompmgr >/dev/null 2>&1; then
		(
			cd /
			exec xcompmgr
		) >/dev/null 2>&1 </dev/null &
	fi

	if [ -e ~/.synergy.conf ]; then
		if which synergys >/dev/null 2>&1; then
			synergys
		fi
	elif which synergyc >/dev/null 2>&1; then
		synergyc
	fi

	if [ -f ~/.wallpaper.bmp ] && which Esetroot >/dev/null 2>&1; then
		nice Esetroot -scale ~/.wallpaper.bmp &
	fi

) &

case "$(hostname)" in
erwin*)
#	exec /usr/bin/gpg-agent --enable-ssh-support --daemon sawfish
	exec /usr/bin/gpg-agent --daemon sawfish
	;;
*)
	exec sawfish
esac

return $?
