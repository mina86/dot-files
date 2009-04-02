##                                                      -*- shell-script -*-
## .zshrc  -- zsh configuration file
## Copyright 2004-2006 by Michal Nazarewicz (mina86/AT/mina86.com)
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
#        promptbang promptpercent promptsubst transientrprimpt
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



##
## Variables
##
unset FCEDIT
NULLCMD=cat
READNULLCMD="$PAGER"
SAVEHIST=500
TMPPREFIX="$TMP/zsh"
HISTFILE="$HOME/.zhistory"



##
## Aliases
##
if ! command_exists xrun; then
	xrun () { "$@" & }
fi

alias -s ps='xrun gv' pdf='xrun xpdf' dvi='xrun xdvi' 'tex=platex' \
         c='gcc' cpp='g++'
alias -s  {jpg,jpeg,png,bmp,gif}=display



##
## Shell options
##
setopt   autocd pushdsilent pushdtohome alwaystoend listtypes alwaystoend \
         noautomenu alwaystoend autolist autoparamkeys autoparamslash \
         globcomplete listambiguous listpacked  listtypes \
         braceccl caseglob casematch equals glob globassign globsubst \
         magicequalsubst nomatch numericglobsort rcexpandparam rematch_pcre \
         shglob unset appendhistory histexpiredupsfirst histfindnodups \
         histignorealldups histignorespace histnofunctions histnostore \
         histreduceblanks histsavenodups globalrcs rcs aliases \
         clobber hashcmds printeightbit rcquotes rmstarsilent bgnice \
         checkjobs hup longlistjobs monitor notify promptsp cbases bsdecho \
         ksharrays kshzerosubscript posixidentifiers shfileexpansion \
         shwordsplit emacs

unsetopt autopushd cdablevars chasedots chaselinks pushdignoredups \
         pushdminus automenu listbeep listrowsfirst menucomplete recexact \
         badpattern bareglobqual cshnullglob extendedglob globdots \
         ignorebraces kshglob markdirs nullglob banghist histallowclobber \
         histbeep histsavebycopy allexport globalexport correct correctall \
         flowcontrol ignoreeof interactivecomments mailwarning pathdirs \
         printexitvalue shortloops promptcr functionargzero multios \
         octalzeroes verbose xtrace cshjunkieloops cshjunkiequotes \
         cshnullcmd kshoptionprint shfileexpansion shnullcmd beep \
         singlelinezle vi autoremoveslash sharehistory

#        alwayslastprompt autonamedirs completealiases hashlistall
#        histsubstpattern multibyte warncreateglobal histverify
#        autocontinue autoresume typesetsilent cshjunkiehistory kshautoload
#        kshtypeset posixbuiltins shoptionletters trapasync overstrike
