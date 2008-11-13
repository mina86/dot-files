
## .xinitrc  -- X init script
## Copyright 2008 by Michal Nazarewicz (mina86/AT/mina86.com)

for DIR in /usr/X11R6/lib/X11/xinit/ "$HOME"; do
	[ -f "$DIR/.Xresources" ]  && xrdb -merge "$DIR/.Xresources"
	[ -f "$DIR/.Xmodmap" ]     && xmodmap     "$DIR/.Xmodmap"
done

xset -b -c r rate 250 50 s off +dpms
cd

case "$(hostname)" in
erwin*)
	( cd /; exec xcompmgr >/dev/null 2>&1 </dev/null ) &
	synergys
#	exec /usr/bin/gpg-agent --enable-ssh-support --daemon sawfish
	exec /usr/bin/gpg-agent --daemon sawfish
	;;
tuptus*)
	[ -e ~/current-bg ] && rm -f -- ~/current-bg
	synergyc erwin
	exec fvwm
	;;
*)
	( cd /; exec xcompmgr >/dev/null 2>&1 </dev/null ) &
	setxkbmap pl
	if [ -f ~/.wallpaper.bmp ] && 
		which Esetroot >/dev/null 2>/dev/null; then
		Esetroot -scale ~/.wallpaper.bmp
	fi
	exec sawfish
esac

return $?
