#!/bin/sh

if [ -r ~/.bashrc ]; then
	. ~/.bashrc

	if declare -f todo >/dev/null 2>&1; then
		todo -v
	fi
fi
