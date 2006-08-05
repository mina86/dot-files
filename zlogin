##                                                      -*- shell-script -*-
## .zlogin  
## Copyright 2004-2006 by Michal Nazarewicz (mina86/AT/mina86.com)
## $Id: zlogin,v 1.1 2006/08/05 22:17:06 mina86 Exp $
##

# Set ENV
export ENV=~/.shellrc

# .zshrc was read
if [ X"$_todo" = Xdefined ]; then
	todo -v
fi
unset _todo;
