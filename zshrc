##                                                      -*- shell-script -*-
## .zshrc  -- zsh configuration file
## Copyright 2004-2006 by Michal Nazarewicz (mina86@mina86.com)
## $Id: zshrc,v 1.2 2006/09/10 11:23:54 mina86 Exp $
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
if [ "$TERM" = 'dumb' ]; then    # EMACS' eshell  (no colors)
	PS1="[%n@%m %30<{<%~]%(!.#.\$) "
elif [ "$TERM" = 'eterm' ]; then # EMACS' term    (ugly line editing)
	E=$'\33['
	PS1="\
%{${E}0;37;44m%}[%{${E}1;32;44m%}%30<{<%~%{${E}0;37;44m%}]\
%{${E}0;1;3%0(?.3.5)m%}%(!.#.\$)%{${E}0m%}%E "
	unset E
else                             # FIXME: I need to check if term sup. colors
	E=$'\33['
	PS1=
	PS1="$PS1%{${E}0;37;44m%}["
	PS1="$PS1%{${E}1;3%(!.1.2);44m%}%n"
	PS1="$PS1%{${E}1;37;44m%}@"
	if [ -z "$SSH_CLIENT$SSH_CONNECTION" ]
	then PS1="$PS1%{${E}1;36;44m%}%m "
	else PS1="$PS1%{${E}1;33;44m%}%m "
	fi
	PS1="$PS1%{${E}1;32;44m%}%50<{<%~"
	PS1="$PS1%{${E}0;37;44m%}]"
	PS1="$PS1%{${E}0;1;3%0(?.3.5)m%}%(!.#.\$)%{${E}0m%} "
	unset E
fi

# Add title to terminals
case "$TERM" in xterm*|rxvt*)
	if [ -z "$SSH_CLIENT$SSH_CONNECTION" ]
	then PS1="$PS1"$'%{\33]2;%~\007%}'
	else PS1="$PS1"$'%{\33]2;%m: %~\007%}'
	fi
esac
export PS1



##
## Variables
##
[ -z "$EDITOR" ] || FCEDIT="$EDITOR"
[ -z "$PAGER" ] || READNULLCMD="$PAGER"
SAVEHIST=500
TMPPREFIX="$TMP/zsh"
HISTFILE="$HOME/.zhistory"
unset NULLCMD



##
## Aliases
##
alias -s  {ps,pdf}=gv dvi=xdvi tex=platex c=gcc cpp=g++
alias -s  {jpg,jpeg,png,bmp,gif}=display



##
## Shell options
##
setopt CDABLE_VARS AUTO_LIST LIST_AMBIGUOUS COMPLETE_IN_WORD		\
	GLOB_COMPLETE AUTO_PARAM_SLASH AUTO_REMOVE_SLASH NO_LIST_BEEP	\
	LIST_PACKED LIST_ROWS_FIRST LIST_TYPES NUMERIC_GLOB_SORT		\
	HIST_IGNORE_ALL_DUPS HIST_IGNORE_SPACE HIST_NO_STORE			\
	HIST_REDUCE_BLANKS HIST_SAVE_NO_DUPS CORRECT MAIL_WARNING		\
	RC_QUOTES NO_BG_NICE PROMPT_SUBST PROMPT_PERCENT SH_WORD_SPLIT	\
	NO_SINGLE_LINE_ZLE EMACS NO_PROMPT_CR
