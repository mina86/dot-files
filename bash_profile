##                                                      -*- shell-script -*-
## .bash_profile
## Copyright 2004-2006 by Michal Nazarewicz (mina86@mina86.com)
## $Id: bash_profile,v 1.5 2008/02/21 08:03:48 mina86 Exp $
##

# Set ENV
export ENV=~/.shellrc

# Source .bashrc
if [ -r ~/.bashrc ]; then
	. ~/.bashrc

	if [ X"$_todo" = Xdefined ]; then
		todo -v
	fi
	unset _todo;
fi

# 64/32 bit differences
export bits=32
if [ x"`uname -m`" = xx86_64 ]; then
	bits=64
fi
for dir in ~/.gkrellm2/plugins ~/.fvwm/modules; do
	if [ -e "$dir$bits" ]; then
		rm "$dir" && ln -s "${dir##*/}$bits" "$dir"
	fi
done
