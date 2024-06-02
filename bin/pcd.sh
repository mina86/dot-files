# shellcheck shell=sh disable=SC2164

cd() {
	case $#:$1 in
	0:)
		command cd
		return
		;;

	1:--help)
		cat <<EOF
Usage:
   cd                                    Move to home directory.
   cd -P                                 Resolve all symbolic links in \$PWD.
   cd [-L | -P | --] <target>            Move to given <target>.
   cd [-L | -P | --] <target> <cmd>...   Run <cmd> in the given <target>.

<target> can be:
   -            Move to directory stored in \$OLDPWD.
   .../<path>   Move to a sibling/cusing directory <path>; this will iterate
                  up the directories until <path> is located.
   <file>       Move to directory containing the file.
   <dir>        Move to given directory.
EOF
		return
		;;

	1:-[LP])
		set -- "$1" "${PWD}"
		;;
	1:*)
		set -- -L "$1"
	esac

	case $1 in
	--)     shift; set -- -L "$@"; ;;
	-[LP])  :; ;;
	*)      set -- -L "$@"
	esac

	if [ $# -eq 2 ]; then
		_cd_do "$@"
	else
		( _cd_do "$1" "$2" && shift 2 && exec "$@" )
	fi
}

_cd_do() {
	case $2 in
	-)
		:
		;;
	.../*)
		set -- "$1" "$2" .. "${2#*/}" "$(stat -Lc%d:%i /)"
		while
			if [ -e "$3/$4" ]; then
				set -- "$1" "$3/$4"
				if ! [ -d "$2" ]; then
					set -- "$1" "$(dirname "$2")"
				fi
				false
			else
				test "$5" != "$(stat -Lc%d:%i "$3")"
			fi
		do
			set -- "$1" "$2" "../$3" "$4" "$5"
		done
		;;
	*)
		if [ -e "$2" ] && ! [ -d "$2" ]; then
			set -- "$1" "$(dirname "$2")"
		fi
	esac
	command cd "$1" -- "$2"
}
