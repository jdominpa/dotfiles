#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Close any open `System Preferences` panes in order to
# avoid overriding the preferences that are being changed.

./close_system_preferences_panes.applescript

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

./app_store.sh
./dock.sh
./finder.sh
./keyboard.sh
./language_and_region.sh
./maps.sh
./safari.sh
./terminal.sh
./textedit.sh
./ui_and_ux.sh
