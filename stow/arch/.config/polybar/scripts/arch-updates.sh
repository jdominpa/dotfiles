#!/bin/sh

updates=$(checkupdates 2>/dev/null | wc -l )

if [ "$updates" -gt 0 ]; then
    echo " $updates"
else
    echo "No updates"
fi
