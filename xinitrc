
## .xinitrc  -- X init script
## Copyright 2008 by Michal Nazarewicz (mina86@mina86.com)

cd
for DIR in /usr/X11R6/lib/X11/xinit/ .; do
	[ -f "$DIR/.Xresources" ]  && xrdb -merge "$DIR/.Xresources"
	[ -f "$DIR/.Xmodmap" ]     && xmodmap     "$DIR/.Xmodmap"
done

if [ -e ~/.xprofile ]; then
	. ~/.xprofile
fi

case "$(hostname)" in
erwin*)
#	exec /usr/bin/gpg-agent --enable-ssh-support --daemon sawfish
	exec /usr/bin/gpg-agent --daemon sawfish
	;;
*)
	exec sawfish
esac

return $?
