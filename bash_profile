##                                                      -*- shell-script -*-
## .bash_profile
## Copyright 2004-2006 by Michal Nazarewicz (mina86/AT/mina86.com)
## $Id: bash_profile,v 1.3 2006/08/05 22:17:06 mina86 Exp $
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
