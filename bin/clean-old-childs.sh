#!/bin/bash
HOUR=$( date +%H )
MINUTE=$( date +%M )
echo "HORA:" $HOUR
if [ "$HOUR" -ne "00" ]; then
  ps -ef | awk -v ppid=$1 -v hour=$HOUR -v minute=$MINUTE -f clean-old-childs.awk
fi
