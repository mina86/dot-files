#!/bin/sh                                               -*- shell-script -*-
## .xinitrc  -- X init script
## Copyright 2004-2006 by Michal Nazarewicz (mina86@mina86.com)
## $Id: xinitrc,v 1.1 2008/05/23 09:22:40 mina86 Exp $
##

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/usr/X11R6/lib/X11/xinit/.Xresources
sysmodmap=/usr/X11R6/lib/X11/xinit/.Xmodmap

[ -f "$sysresources" ]  && xrdb -merge "$sysresources"
[ -f "$userresources" ] && xrdb -merge "$userresources"
[ -f "$sysmodmap" ]     && xmodmap "$sysmodmap"
[ -f "$usermodmap" ]    && xmodmap "$usermodmap"

xset b on -c r rate 250 50 s off
xcompmgr >/dev/null 2>&1 &
cd

#exec /usr/bin/gpg-agent --enable-ssh-support --daemon fvwm
exec /usr/bin/gpg-agent --daemon fvwm
return $?
