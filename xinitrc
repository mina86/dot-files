
## .xinitrc  -- X init script
## Copyright 2008 by Michal Nazarewicz (mina86@mina86.com)

cd
for DIR in /usr/X11R6/lib/X11/xinit/ .; do
	[ -f "$DIR/.Xresources" ]  && xrdb -merge "$DIR/.Xresources"
	[ -f "$DIR/.Xmodmap" ]     && xmodmap     "$DIR/.Xmodmap"
done

xset -b -c r rate 250 50 s off +dpms


run_xcompmgr () {
	if which xcompmgr >/dev/null 2>&1; then
		(
			cd /
			exec xcompmgr >/dev/null 2>&1 </dev/null
		) &
	fi
}


case "$(hostname)" in
erwin*)
	run_xcompmgr
	synergys
#	exec /usr/bin/gpg-agent --enable-ssh-support --daemon sawfish
	exec /usr/bin/gpg-agent --daemon sawfish
	;;
*)
	run_xcompmgr
	if [ -f ~/.wallpaper.bmp ] && which Esetroot >/dev/null 2>/dev/null; then
		nice Esetroot -scale ~/.wallpaper.bmp &
	fi
	exec sawfish
esac

return $?
