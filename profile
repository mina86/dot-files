##                                                      -*- shell-script -*-
## .profile
## Copyright 2004-2006 by Michal Nazarewicz (mina86@mina86.com)
## $Id: profile,v 1.1 2006/08/05 22:17:06 mina86 Exp $
##

# Set ENV
export ENV=~/.shellrc

# Source .shellrc
if [ -r ~/.shellrc ]; then
	. ~/.shellrc

	if [ X"$_todo" = Xdefined ]; then
		todo -v
	fi
	unset _todo;
fi
