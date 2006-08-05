##                                                      -*- shell-script -*-
## .bashrc  -- bash configuration file
## Copyright 2004-2006 by Michal Nazarewicz (mina86@mina86.com)
## $Id: bashrc,v 1.4 2006/08/05 22:17:06 mina86 Exp $
##

# Include ~/.shellrc
if [ -r ~/.shellrc ]; then
	. ~/.shellrc
fi

# Not interactive?
[ X"${-#*i}" != X"$-" ] || return


##
## Truncate PWD
##
trunc_pwd () {
	set -- "${1-30}" "${2-...}" "$3" "${PWD/#$HOME/~}" "$?"
	if [ ${#4} -le $1 ]
	then printf %s "$4"
	else printf %s%s "$2" "${4:$(( ${#4} - $1 + ${3-${#2}} ))}"
	fi
	return $5
}


##
## Prompt
##
_remote=y; [ -n "$SSH_CLIENT$SSH_CONNECTION" ] || _remote=
if [ X"$TERM" = Xdumb ]; then    # EMACS' eshell  (no colors)
	PS1='[\u@\h $(trunc_pwd 30 {)]\$ '
elif [ X"$TERM" = Xeterm ]; then # EMACS' term    (ugly line editing)
	export PS1='\
\[\033[1;32;44m\]$(trunc_pwd 30 {)\
\[\033[0;1;33m\]\$\[\033[0m\] '
else                             # FIXME: I need to check if term sup. colors
	PS1='\
\[\033[0;37;44m\][\
\[\033[1;3$(($UID?2:1));44m\]\u\
\[\033[1;37;44m\]@'
	if [ -z "$_remote" ]; then
		PS1="$PS1"'\[\033[1;36;44m\]\h '
	else
		PS1="$PS1"'\[\033[1;33;44m\]\h '
	fi
	PS1="$PS1"'\
\[\033[1;32;44m\]$(trunc_pwd 30 {)\
\[\033[0;37;44m\]]\
\[\033[0;1;3$(($??5:3))m\]\$\[\033[0m\] '
fi

# Add title to terminals
case "$TERM" in
xterm*|rxvt*)
	if [ -z "$_remote" ]; then
		PS1="$PS1"'\[\033]2;\w\007\]'
	else
		PS1="$PS1"'\[\033]2;\h: \w\007\]'
	fi
	;;
esac
export PS1
unset PROMPT_COMMAND _remote




##
## Shell optons and parameters
##
shopt -qu cdable_vars checkhash dotglob execfail extdebug extglob
shopt -qu force_fignore interactive_comments lithist mailwarn
shopt -qu no_empty_cmd_completion nocaseglob shift_verbose
shopt -qu sourcepath xpg_echo

shopt -qs cdspell checkwinsize cmdhist expand_aliases failglob
shopt -qs extquote gnu_errfmt histappend histreedit histverify hostcomplete
shopt -qs nullglob promptvars

HISTCONTROL="ignorespace:erasedups"
HISTIGNORE="ls:su:cd:bc:wp rm:mp3 mv:cd -"
