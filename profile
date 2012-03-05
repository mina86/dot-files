##                                                      -*- shell-script -*-
## .profile
## Copyright 2004-2008 by Michal Nazarewicz (mina86@mina86.com)
##

# Set ENV
export ENV=~/.shellrc

# Auto logout on time out
TMOUT=1200

# Source .shellrc
if [ x"$1" != xnothing ]; then
	if [ -n "$1" ] && [ -r "$1" ]; then
		. "$1"
	elif [ -r ~/.shellrc ]; then
		. ~/.shellrc
	fi
fi

# 64/32 bit differences
bits=32
if [ x"`uname -m`" = xx86_64 ]; then
	bits=64
fi
for dir in ~/.gkrellm2/plugins ~/.fvwm/modules ~/.irssi/modules; do
	if [ -e "$dir$bits" ]; then
		rm "$dir" && ln -s "${dir##*/}$bits" "$dir"
	fi
done

# Start emacs if not running
if ! [ -e /tmp/emacs$UID/server ]; then
	xrun emacs --daemon
fi
