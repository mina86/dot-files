
## .xinitrc  -- X init script
## Copyright 2008 by Michal Nazarewicz (mina86/AT/mina86.com)
## $Id: xinitrc,v 1.4 2008/06/04 21:45:25 mina86 Exp $
##

# Hostname
if [ -z "$HOST" ]; then
	_hostname="$(hostname)"
	_hostname="X${_hostname%%.*}"
else
	_hostname=X"$HOST"
fi

for DIR in /usr/X11R6/lib/X11/xinit/ "$HOME"; do
	[ -f "$DIR/.Xresources" ]  && xrdb -merge "$DIR/.Xresources"
	[ -f "$DIR/.Xmodmap" ]     && xmodmap     "$DIR/.Xmodmap"
done

xset -b -c r rate 250 50 s off +dpms
cd
if [ "$_hostname" = Xerwin ]; then
	xcompmgr >/dev/null 2>&1 &
	synergys
#	exec /usr/bin/gpg-agent --enable-ssh-support --daemon fvwm
	exec /usr/bin/gpg-agent --daemon fvwm
else
	[ -e ~/current-bg ] && rm -f -- ~/current-bg
	synergyc erwin
	exec fvwm
fi
return $?
