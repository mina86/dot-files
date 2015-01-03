#!/bin/sh

vol=$(amixer get Master | grep -o '[0-9][0-9]*%' | tr -d %)
if [ $vol -lt 30 ] || [ $vol -ge 60 ]; then
    vol=30
else
    vol=$(( (($vol / 10 - 2) % 4 + 3) * 10 ))
fi
amixer set Master $vol% |tail -n1
