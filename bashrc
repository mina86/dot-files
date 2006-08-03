##                                                      -*- shell-script -*-
## .bashrc  -- bash configuration file
## Copyright 2004-2006 by Michal Nazarewicz (mina86@mina86.com)
## $Id: bashrc,v 1.3 2006/08/03 13:26:25 mina86 Exp $
##

# Not interactive?
[ X"${-#*i}" != X"$-" ] || return


# Hostname
_hostname="`hostname|tr ABCDEFGHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz`"
_hostname=X"${_hostname%%.*}"
if [ "$_hostname" = Xikar ]; then _hostname=Xdedal; fi
_remote=y; [ -n "$SSH_CLIENT$SSH_CONNECTION" ] || _remote=



##
## Truncate PWD
##
trunc_pwd () {
	local EC=$?
	local LEN=${1-30}
	local STR="${PWD/#$HOME/~}"

	# Bash bug workaround (after `cd //`  $PWD is '//')
	if [ "$STR" == "//" ]; then printf /; return $EC; fi

	if [ ${#STR} -le $LEN ]; then
		printf %s "$STR"
		return $EC
	fi

	local TRUNC=${2-...}
	printf %s%s "$TRUNC" "${STR:$(( ${#STR} - $LEN + ${3-${#TRUNC}} ))}"
	return $EC
}


##
## make and change directory
##
md () {
	if [ $# -eq 0 ]; then
		echo usage: 'md dir [ dir ... ]'
		return 1
	elif [ $# -eq 1 ]; then
		mkdir -p -- "$1" && cd -- "$1"
	else
		mkdir -p -- "$@"
	fi
}


##
## Calculator
##
if which bc >/dev/null 2>&1; then
	c () {
		printf 'scale = 4; \n%s\n' "$*" | bc
	}
fi


##
## Prompt
##
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


##
## Other
##
if [ -d "$HOME/bin" ] &&
		! printf %s "$PATH" | grep -F "$HOME/bin" -F '~/bin' >/dev/null 2>&1; then
	PATH="$PATH:$HOME/bin"
fi
if [ -d "$HOME/code/tinyapps" ] &&
		! printf '%s' "$PATH" | grep -F "$HOME/code/tinyapps" >/dev/null 2>&1; then
	PATH="$PATH:$HOME/code/tinyapps"
fi
if which nano >/dev/null 2>/dev/null; then
	export EDITOR=nano VISUAL=nano
fi
if which less >/dev/null 2>/dev/null; then
	export PAGER=less
fi

export CVS_RSH=ssh
if [ -d "$HOME/.cvsroot" ]; then
	export CVSROOT=/home/mina86/.cvsroot
fi

if [ "$_hostname" = Xerwin ]; then
	export LANG=pl_PL.ISO8859-2 LC_COLLATE=C LC_MESSAGES=en_GB
	export __GL_FSAA_MODE=4 __GL_LOG_MAX_ANISO=1 __GL_SYNC_TO_VBLANK=1
elif [ "$_hostname" = Xtuptus ]; then
	export LANG=pl_PL.ISO8859-2 LC_COLLATE=C LC_MESSAGES=en_GB
fi


##
## Compilation
##
export CC=gcc
export CXX=g++

if [ "$_hostname" = Xerwin ]; then   # My home workstation
	export ARCH="i686"
	export HOST="${ARCH}-slackware-linux-gnu"
	#export ARCH="x86_64"
	#export HOST="${ARCH}-pc-linux-gnu"

	FLAGS="-march=athlon64 -pipe -DNDEBUG -DG_DISABLE_ASSERT -s"
	FLAGS="$FLAGS -O3 -falign-loops=1 -falign-labels=1 -frename-registers"
	FLAGS="$FLAGS -funit-at-a-time"
	export LDFLAGS="-s -z combreloc"

	# Export FLAGS
	for L in C CPP CXX; do
		export ${L}ARCH="$ARCH"
		export ${L}HOST="$HOST"
		export ${L}FLAGS="$FLAGS"
	done

elif [ "$_hostname" = Xtuptus ]; then   # My notebook
	export ARCH="i686"
	export HOST="${ARCH}-slackware-linux-gnu"

	FLAGS="-march=athlon-4 -pipe -DNDEBUG -DG_DISABLE_ASSERT -s"
	FLAGS="$FLAGS -O3 -falign-loops=1 -falign-labels=1 -frename-registers"
	FLAGS="$FLAGS -funit-at-a-timeb"
	export LDFLAGS="-s -z combreloc"

	# Export FLAGS
	for L in C CPP CXX; do
		export ${L}ARCH="$ARCH"
		export ${L}HOST="$HOST"
		export ${L}FLAGS="$FLAGS"
	done

elif [ "X$(uname -m)" = Xsun4u ]; then   # Mion, Ikar or Dedal
	FLAGS="-march=sparc -pipe -DNDEBUG -DG_DISABLE_ASSERT -s"
	FLAGS="$FLAGS -O3 -falign-loops=1 -falign-labels=1 -frename-registers"
	FLAGS="$FLAGS -funit-at-a-time"
	export LDFLAGS="-s -z combreloc"

	for L in C CPP CXX; do
		export ${L}FLAGS="$FLAGS"
	done
fi

unset FLAGS

# Use  -fomit-frame-pointer  only with C
# Decrese inline-limit for C only since there
# are tons of  inline functions in C++
CFLAGS="$FLAGS -fomit-frame-pointer -finline-limit=400"


##
## Aliases
##

# LS
if [ -z "$LS_COLORS" ]; then
	if which dircolors >/dev/null 2>&1; then
		if [ -f "$HOME/.dir_colors" ]; then
			eval `dircolors -b $HOME/.dir_colors`
		elif [ -f /etc/DIR_COLORS ]; then
			eval `dircolors -b /etc/DIR_COLORS`
		else
			eval `dircolors -b`
		fi
	elif [ -x ~/bin/dircolors ]; then
		. ~/bin/dircolors
	fi
fi
if [ -z "$LS_OPTIONS" ]; then
	export LS_OPTIONS="-FbT0 --color=auto"
fi

# Ikar, Dedal & Mion keep GNU ls in /usr/local/bin/ls
if [ -x /usr/local/bin/ls ]; then
	alias ls='LANG=C /usr/local/bin/ls $LS_OPTIONS'
else
	alias ls="LANG=C '$(which ls)' $LS_OPTIONS"
fi

# Other
if which run-emacs >/dev/null 2>&1; then
	alias e="run-emacs"
fi
alias mb=mv
alias rmd=rmdir
if [ "$_hostname" = Xerwin ] || [ "X$_hostname" = Xtuptus ]; then
	alias mpc="mpc --format '[[[%artist% <&%album%> ]|[%artist% - ]|[<%album%> ]]%title%]|[%file%]'"
fi

# Links and ELinks
ELINKS=`which elinks 2>/dev/null`
LINKS=`which links 2>/dev/null`
if [ -n "$ELINKS" ]; then
	alias links="$ELINKS"
elif [ -n "$LINKS" ]; then
	alias elinks="$LINKS"
fi
unset ELINKS LINKS

# Preserve root if available
if rm --help 2>/dev/null | grep ' --preserve-root' >/dev/null 2>&1; then
	alias rm="`which rm` --preserve-root"
fi


##
## Shell optons and parameters
##
shopt -qu cdable_vars dotglob execfail extdebug extglob force_fignore
shopt -qu interactive_comments lithist no_empty_cmd_completion nocaseglob
shopt -qu nullglob shift_verbose sourcepath xpg_echo
shopt -qs cdspell checkhash checkwinsize cmdhist expand_aliases failglob
shopt -qs extquote gnu_errfmt histappend histverify hostcomplete promptvars

FIGNORE=".o:~:.bak"
HISTCONTROL="ignorespace:erasedups"
HISTIGNORE="ls:su:cd:bc:wp rm:mp3 mv:cd -"
if [ "$_hostname" = Xerwin ]; then
	unset MAILCHECK
fi


##
## stty
##
#stty stop '' start '' lnext ^Q


##
## TODO List
##
if [ "$_hostname" = Xerwin ]; then
	todo () {
		case "${1-view}" in
		(-c|clearview) clear; todo -v; ;;
		(-v|view)
			if [ -s ~/.todo ]
			then cat ~/.todo
			else echo Nothing to do
			fi; ;;
		(-e|edit) "${EDITOR-${VISUAL-vi}}" ~/.todo; ;;
		(-d|delete) rm -i ~/.todo; ;;
		esac
	}
fi


##
## Source per-host .shellrc
##
if [ -r ~/.shellrc-"${_hostname#X}" ]; then . ~/.shellrc-"${_hostname#X}"; fi



unset _hostname _remote
