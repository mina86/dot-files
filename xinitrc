
## .xinitrc  -- X init script
## Copyright 2008 by Michal Nazarewicz (mina86@mina86.com)
## $Id: xinitrc,v 1.5 2008/07/29 14:03:11 mina86 Exp $
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
	( cd /; xcompmgr >/dev/null 2>&1 </dev/null & ) &
	synergys
#	exec /usr/bin/gpg-agent --enable-ssh-support --daemon fvwm
	exec /usr/bin/gpg-agent --daemon fvwm
elif [ "$_hostname" = Xtuptus ]; then
	[ -e ~/current-bg ] && rm -f -- ~/current-bg
	synergyc erwin
	exec fvwm
else
	( cd /; xcompmgr >/dev/null 2>&1 </dev/null & ) &
	exec fvwm
fi
return $?
