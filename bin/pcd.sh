# shellcheck shell=sh disable=SC2164

# Powerful cd, a cd command replacement
# Copyright 2024 Michał Nazarewicz <mina86@mina86.com>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the “Software”), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.


# A ‘cd’ command replacement with additional features.  To use source this file
# and proceed using ‘cd’ as you’ve been doing before.  To install permanently,
# add sourcing of the file to ~/.bashrc, ~/.shellrc or equivalent for your
# choice of shell.
#
# See https://mina86.com/2024/powerful-cd/ for more detailed description.


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
