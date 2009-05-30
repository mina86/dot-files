##                                                      -*- shell-script -*-
## .zshrc  -- zsh configuration file
## Copyright 2004-2009 by Michal Nazarewicz (mina86/AT/mina86.com)
##


# Include ~/.shellrc
if [ -r ~/.shellrc ]; then
	. ~/.shellrc
fi


# Not interactive?
[ X"${-#*i}" != X"$-" ] || return


##
## Enable arraies of functions
##
typeset -ga preexec_functions
typeset -ga precmd_functions
typeset -ga chpwd_functions



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
	PS1+="%{${E}0;37;44m%}["              # "["
	PS1+="%{${E}1;3%(!.1.2);44m%}%n"      # user
	PS1+="%{${E}1;37;44m%}@"              # "@"
	case "$SSH_CLIENT$SSH_CONNECTION" in  # host
	?*) PS1+="%{${E}1;33;44m%}%m " ;;
	*)  PS1+="%{${E}1;36;44m%}%m " ;;
	esac
	PS1+="{{GIT}}"                        # git info
	PS1+="%{${E}1;32;44m%}%30<{<%~"       # dir
	PS1+="%{${E}0;37;44m%}]"              # "]"
	PS1+="%{${E}0;1;3%0(?.3.5)m%}%(!.#.\$)%{${E}0m%} "
	unset E
fi


##
## Prompt GIT Stuff
##

# Sources
# * http://www.jukie.net/~bart/conf/zsh.d/S55_git
# * http://blog.madism.org/index.php/2008/05/07/173-git-prompt
# * http://github.com/jcorbin/zsh-git/
# * and of course rewritten a lot by myself

if command_exists git; then
	__PS1=$PS1

	__update_ps1_git () {
	local dir

	if ! dir=$(git rev-parse --git-dir 2>/dev/null); then
		PS1=${__PS1//{{GIT}}/}
	else
		local branch state E ps flags _flags
		state=
		if   [ -d "$dir/rebase-apply" ] ; then
			if   [ -f "$dir/rebase-apply/rebasing" ]; then state="rb"
			elif [ -f "$dir/rebase-apply/applying" ]; then state="am"
			else                                           state="am/rb"
			fi
			branch="$(git symbolic-ref HEAD 2>/dev/null)"
		elif [ -f "$dir/rebase-merge/interactive" ]; then
			state="rb-i"
			branch="$(cat "$dir/rebase-merge/head-name")"
		elif [ -d "$dir/rebase-merge" ]; then
			state="rb-m"
			branch="$(cat "$dir/rebase-merge/head-name")"
		elif [ -f "$dir/MERGE_HEAD" ]; then
			state="mrg"
			branch="$(git symbolic-ref HEAD 2>/dev/null)"
		else
			[ -f "$dir/BISECT_LOG" ] && state="bisect"
			branch="$(git symbolic-ref HEAD 2>/dev/null)" || \
				branch="$(git describe --exact-match HEAD 2>/dev/null)" || \
				branch="$(cut -c1-7 "$dir/HEAD")..."
		fi
		branch="${branch#refs/heads/}"

		# Takes ages on big repositories
# 		_flags=$(git status 2>/dev/null | sed -ne '
# s/^# Untracked files:.*/u/p
# s/^# Changes to be committed:.*/i/p
# s/^# Changed but not updated:.*/m/p
# s/^# [[:space:]]*unmerged:.*/c/p')
# 		flags=
# 		case "$_flags" in *'i'*) flags+='+'                    ; esac
# 		case "$_flags" in *"c"*) flags+='!';; *"m"*) flags+='*'; esac
# 		case "$_flags" in *'u'*) flags+='?'                    ; esac

		E=$'\33['
		case "$flags$state" in                 # branch
		?*) ps1="%{${E}1;32;44m%}$branch" ;;
		*)  ps1="%{${E}0;32;44m%}$branch"
		esac
		case "$state" in ?*)                   # (state)
			ps1+="%{${E}0;37;44m%}("
			ps1+="%{${E}0;32;44m%}$state"
			ps1+="%{${E}0;37;44m%})"
		esac
		# case "$flags" in ?*)                   # flags
		# 	ps1+="%{${E}1;31;44m%}$flags%{${E}0;37;44m%}"
		# esac

		PS1=${__PS1//{{GIT}}/"$ps1 "}
	fi
	unset __PS1_GIT_INVALID
	}

	__invalidate_ps1_git_maybe () {
		case "$1" in *git*)
			__PS1_GIT_INVALID=yes
		esac
	}

	__update_ps1_git_maybe () {
		case "$__PS1_GIT_INVALID" in yes)
			__update_ps1_git
		esac
	}

	chpwd_functions+=__update_ps1_git
	preexec_functions+=__invalidate_ps1_git_maybe
	precmd_functions+=__update_ps1_git_maybe
	__update_ps1_git
else
	PS1=${PS1//{{GIT}}/}
fi


##
## Add title to terminals
##
#		screen) print -Pn "\ek$a:$3\e\\"      # screen title (in ^A")
case $TERM in xterm*|rxvt*)
	__title() {
		local a="${(V)1//\%/\%\%}"
		a="$(print -Pn "%40>...>$a" | tr -d "\n")"
		print -Pn "\e]2;[$2$3]%(!.#.\$) $a\a"
	}

	case "$(id -nu)@$SSH_CLIENT$SSH_CONNECTION" in
	mina86@)    _title_=           ;;
	mina86@?*)  _title_='@%m '     ;;
	*@)         _title_="$USER "   ;;
	*@?*)       _title_="$USER@%m ";;
	esac

	eval "__preexec_title() { __title \"\$1\" \"$_title_\" \"%40<...<%~\" }"
	preexec_functions+=__preexec_title
	precmd_functions+=__preexec_title
	unset _title_
esac



##
## Variables
##
unset FCEDIT
NULLCMD=cat
READNULLCMD="$PAGER"
SAVEHIST=1000
HISTSIZE=1000
TMPPREFIX="$TMP/zsh"
HISTFILE="$HOME/.zhistory"



##
## Shell options
##
setopt   autocd pushdsilent pushdtohome alwaystoend listtypes alwaystoend \
         noautomenu alwaystoend autolist autoparamkeys autoparamslash \
         globcomplete listpacked  listtypes \
         braceccl caseglob casematch equals glob globassign globsubst \
         magicequalsubst nomatch numericglobsort rcexpandparam rematch_pcre \
         shglob unset appendhistory histexpiredupsfirst histfindnodups \
         histignorealldups histignorespace histnofunctions histnostore \
         histreduceblanks histsavenodups globalrcs rcs aliases \
         clobber hashcmds printeightbit rcquotes rmstarsilent bgnice \
         checkjobs hup longlistjobs monitor notify promptsp cbases bsdecho \
         ksharrays kshzerosubscript posixidentifiers shfileexpansion \
         shwordsplit emacs banghist

unsetopt autopushd cdablevars chasedots chaselinks pushdignoredups \
         pushdminus automenu listbeep listrowsfirst menucomplete recexact \
         badpattern bareglobqual cshnullglob extendedglob globdots \
         ignorebraces kshglob markdirs nullglob histallowclobber \
         histbeep histsavebycopy allexport globalexport correct correctall \
         flowcontrol ignoreeof interactivecomments mailwarning pathdirs \
         printexitvalue shortloops promptcr functionargzero multios \
         octalzeroes verbose xtrace cshjunkieloops cshjunkiequotes \
         cshnullcmd kshoptionprint shfileexpansion shnullcmd beep \
         singlelinezle vi autoremoveslash sharehistory listambiguous

bindkey -e


##
## Completion
##

# Completion stuff
zmodload -i zsh/complist
zstyle ':completion:*' completer _expand _complete _ignored
zstyle ':completion:*' expand prefix suffix
zstyle ':completion:*' file-sort name
zstyle ':completion:*' glob 1
zstyle ':completion:*' ignore-parents parent pwd .. directory
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' remove-all-dups true
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' verbose yes

zstyle ':completion:*:processes' command 'ps -aU$USER'
zstyle ':completion:*:processes' menu select 1
zstyle ':completion:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'

zstyle ':completion:*:manuals' verbose no
zstyle ':completion:*:manuals' menu select 0

zstyle ':completion:*:functions' ignored-patterns '_*'

zstyle ':completion:*:*:configure:*' menu select 1

zstyle ':completion:*:*:ssh:*' tag-order hosts
if [ "0$(grep -c ^Host ~/.ssh/config 2>/dev/null)" -gt 3 ]; then
	zstyle ':completion:*:*:ssh:*' hosts 'reply=(
  ${="$(sed -ne "s/^Host[[:space:]][[:space:]]*\\([^*]\\)/\1/p" ~/.ssh/config)"}
)'
fi

# formatting and messages
zstyle ':completion:*:descriptions' format '>> %B%d%b <<'
zstyle ':completion:*:messages'     format '%d'
zstyle ':completion:*:warnings'     format 'No matches for: %d'
zstyle ':completion:*:corrections'  format '%B%d (errors: %e)%b'
zstyle ':completion:*' group-name ''

# offer indexes before parameters in subscripts
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters
zstyle ':completion:*:*:*:*:*files' ignored-patterns '*.o' '*~' \
	'*.old' '*.bak'

# ignore completion functions (until the _ignored completer)
# zstyle ':completion:*:scp:*' tag-order \
# 	files users 'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'
# zstyle ':completion:*:scp:*' group-order \
# 	files all-files users hosts-domain hosts-host hosts-ipaddr
# zstyle ':completion:*:ssh:*' tag-order \
# 	users 'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'
# zstyle ':completion:*:ssh:*' group-order \
# 	hosts-domain hosts-host users hosts-ipaddr

autoload -U compinit
compinit


##
## Bindings
##
bindkey '^D' delete-char
