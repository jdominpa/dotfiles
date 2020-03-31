#!/bin/sh

updates=$(checkupdates 2>/dev/null | wc -l )

if [ "$updates" -gt 0 ]; then
    echo "$updates packages"
else
    echo "No updates"
fi
