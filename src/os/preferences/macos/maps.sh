#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Maps\n\n"

execute "defaults write com.apple.Maps LastClosedWindowViewOptions '{
            mapType = 0;          // show hybrid map
            trafficEnabled = 0;   // do not show traffic
         }'" \
    "Set view options"

killall "Maps" &> /dev/null
