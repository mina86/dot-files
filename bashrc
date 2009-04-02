##                                                      -*- shell-script -*-
## .bashrc  -- bash configuration file
## Copyright 2004-2008 by Michal Nazarewicz (mina86@mina86.com)
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

unset PROMPT_COMMAND P




##
## Shell optons and parameters
##
shopt -qu cdable_vars checkhash dotglob execfail extglob interactive_comments
shopt -qu interactive_comments lithist no_empty_cmd_completion nocaseglob
shopt -qu shift_verbose sourcepath xpg_echo

shopt -qs cdspell checkwinsize cmdhist expand_aliases histappend histreedit
shopt -qs histverify hostcomplete nullglob promptvars huponexit

if [ "$_hostname" != Xdedal ]; then
	shopt -qu extdebug force_fignore
	shopt -qs failglob extquote gnu_errfmt
fi

HISTCONTROL="ignorespace:erasedups"
HISTIGNORE="ls:su:cd:bc:wp rm:mp3 mv:cd -"
