##                                                      -*- shell-script -*-
## .bashrc  -- bash configuration file
## Copyright 2004-2006 by Michal Nazarewicz (mina86/AT/mina86.com)
## $Id: bash_profile,v 1.2 2006/08/03 13:26:25 mina86 Exp $
##

# Source per-host .profile
h="$(hostname|tr ABCDEFGHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz)"
h="${h%%.*}"; if [ X"$h" = Xikar ]; then h=dedal; fi
if [ -r ~/.profile-"$h" ]; then . ~/.profile-"$h"; fi
unset h

# Source .bashrc
if [ -r ~/.bashrc ]; then
	. ~/.bashrc

	if declare -f todo >/dev/null 2>&1; then
		todo -v
	fi
fi
