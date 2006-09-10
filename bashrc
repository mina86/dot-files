##                                                      -*- shell-script -*-
## .bashrc  -- bash configuration file
## Copyright 2004-2006 by Michal Nazarewicz (mina86@mina86.com)
## $Id: bashrc,v 1.7 2006/09/10 10:43:44 mina86 Exp $
##

# Include ~/.shellrc
if [ -r ~/.shellrc ]; then
	. ~/.shellrc
fi

# Not interactive?
[ X"${-#*i}" != X"$-" ] || return


##
## Prompt
##

# tpwd
if declare -f tpwd >/dev/null 2>&1; then
	P='$(tpwd -n 30 {)'
elif P="$(which tpwd 2>/dev/null)"; then
	eval "$("$P" --bash)"
	P='$(tpwd -n 30 {)'
else
	P='\w'
fi


if [ X"$TERM" = Xdumb ]; then    # EMACS' eshell  (no colors)
	PS1='[\u@\h '"$P"']\$ '
elif [ X"$TERM" = Xeterm ]; then # EMACS' term    (ugly line editing)
	PS1='\
\[\033[1;32;44m\]'"$P"'\
\[\033[0;1;33m\]\$\[\033[0m\] '
else                             # FIXME: I need to check if term sup. colors
	PS1='\
\[\033[0;37;44m\][\
\[\033[1;3$(($UID?2:1));44m\]\u\
\[\033[1;37;44m\]@'
	if [ -z "$SSH_CLIENT$SSH_CONNECTION" ]
	then PS1="$PS1"'\[\033[1;36;44m\]\h '
	else PS1="$PS1"'\[\033[1;33;44m\]\h '
	fi
	PS1="$PS1"'\
\[\033[1;32;44m\]'"$P"'\
\[\033[0;37;44m\]]\
\[\033[0;1;3$(($??5:3))m\]\$\[\033[0m\] '
fi

# Add title to terminals
case "$TERM" in xterm*|rxvt*)
	if [ -z "$SSH_CLIENT$SSH_CONNECTION" ]
	then PS1="$PS1"'\[\033]2;\w\007\]'
	else PS1="$PS1"'\[\033]2;\h: \w\007\]'
	fi
esac

export PS1
unset PROMPT_COMMAND P




##
## Shell optons and parameters
##
shopt -qu cdable_vars checkhash dotglob execfail extdebug extglob
shopt -qu force_fignore interactive_comments lithist
shopt -qu no_empty_cmd_completion nocaseglob shift_verbose
shopt -qu sourcepath xpg_echo

shopt -qs cdspell checkwinsize cmdhist expand_aliases failglob
shopt -qs extquote gnu_errfmt histappend histreedit histverify hostcomplete
shopt -qs nullglob promptvars

HISTCONTROL="ignorespace:erasedups"
HISTIGNORE="ls:su:cd:bc:wp rm:mp3 mv:cd -"
