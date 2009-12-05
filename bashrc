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

# Bash seems to have some problems with TERM=rxvt-unicode, probably
# due to invalid terminfo file or some such. I'll need to investigate
# it further when I have the time.
case "$TERM" in rxvt*)
	TERM=xterm
esac


# path
if declare -f tpwd >/dev/null 2>&1; then
	P='$(tpwd -n 30 {)'
elif P="$(which tpwd 2>/dev/null)"; then
	eval "$("$P" --bash)"
	P='$(tpwd -n 30 {)'
else
	P='\w'
fi


PS1=
case "$TERM" in
dumb)                   # EMACS' eshell  (no colors)
	PS1+='[\u@\h {{GIT}}'"$P"']\$ '
	;;
*)                      # FIXME: I need to check if term sup. colors
	if [ x"$TERM" != xeterm ]; then
		PS1+='\[\e[0;37;44m\]['
		PS1+='\[\e[1;3$(($UID?2:1));44m\]\u'
		PS1+='\[\e[1;37;44m\]@'
		if [ -z "$SSH_CLIENT$SSH_CONNECTION" ]
		then PS1+='\[\e[1;36;44m\]\h '
		else PS1+='\[\e[1;33;44m\]\h '
		fi
	fi
	PS1+='{{GIT}}'
	PS1+='\[\e[1;32;44m\]'
	PS1+="$P"
	PS1+='\[\e[0;37;44m\]]'
	PS1+='\[\e[0;1;3$(($??5:3))m\]\$\[\e[0m\] '
esac

# Add title to terminals
case "$TERM" in xterm*|rxvt*)
	PS1+='\[\e]2;'
	case "$(id -nu)@$SSH_CLIENT$SSH_CONNECTION" in
	mina86@)   PS1+='(\s)' ;;
	mina86@*)  PS1+='@\h'  ;;
	*@)        PS1+='\u'   ;;
	*@*)       PS1+='\u@\u';;
	esac
	PS1+=' \w\007\]'
esac
PS2=':; '

unset PROMPT_COMMAND P
export PS1 PS2



##
## Prompt GIT Stuff
##

# Sources
# * http://www.jukie.net/~bart/conf/zsh.d/S55_git
# * http://blog.madism.org/index.php/2008/05/07/173-git-prompt
# * http://github.com/jcorbin/zsh-git/
# * and of course rewritten a lot by myself

if which git >/dev/null 2>&1; then
	__PS1=$PS1
	__git_prompt () {
		case "$PWD" in "$__GIT_PROMPT_PWD" )
			return 0
		esac

		__GIT_PROMPT_PWD=$PWD
		local dir
		dir=$__GIT_PROMPT_GITDIR
		if ! __GIT_PROMPT_GITDIR=$(git rev-parse --git-dir 2>/dev/null); then
			PS1=${__PS1//'{{GIT}}'/}
			return 0
		fi

		case "$__GIT_PROMPT_GITDIR" in [^/]*)
				__GIT_PROMPT_GITDIR=$PWD/$__GIT_PROMPT_GITDIR
		esac
		case "$dir" in "$__GIT_PROMPT_GITDIR")
			return 0
		esac

		local branch state E ps flags _flags
		state=
		dir=$__GIT_PROMPT_GITDIR
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

		case "$flags$state" in                 # branch
		?*) ps1="\[\e[1;32;44m\]$branch" ;;
		*)  ps1="\[\e[0;32;44m\]$branch"
		esac
		case "$state" in ?*)                   # (state)
			ps1+="\[\e[0;37;44m\]("
			ps1+="\[\e[0;32;44m\]$state"
			ps1+="\[\e[0;37;44m\])"
		esac
		# case "$flags" in ?*)                   # flags
		# 	ps1+="\[\e[1;31;44m\]$flags\[\e[0;37;44m\]"
		# esac

		PS1=${__PS1//'{{GIT}}'/"$ps1 "}
	}

	unalias g
	function g () {
		if [ $# -eq 0 ]; then
			unset __GIT_PROMPT_PWD __GIT_PROMPT_GITDIR
		else
			git "$@"
		fi
	}

	PROMPT_COMMAND='__git_prompt'
else
	PS1=${PS1//'{{GIT}}'/}
fi



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
