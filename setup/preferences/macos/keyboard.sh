#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Keyboard\n\n"

execute "defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false" \
    "Disable press-and-hold in favor of key repeat"

execute "defaults write NSGlobalDomain InitialKeyRepeat -int 10" \
    "Set delay until repeat"

execute "defaults write NSGlobalDomain KeyRepeat -int 1" \
    "Set the key repeat rate to fast"

execute "defaults write NSGlobalDomain NSAutomaticCapitalizationEnabled -bool false" \
    "Disable automatic capitalization"

execute "defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false" \
    "Disable automatic correction"

execute "defaults write NSGlobalDomain NSAutomaticPeriodSubstitutionEnabled -bool false" \
    "Disable automatic period substitution"

execute "defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false" \
    "Disable smart dashes"

execute "defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false" \
    "Disable smart quotes"
