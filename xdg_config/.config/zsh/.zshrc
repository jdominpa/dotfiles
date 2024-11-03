#!/usr/bin/env zsh
#
# Completion
#

fpath=($ZDOTDIR/completions $fpath)

autoload -Uz compinit
compinit -u

# Make completion:
# - Try exact (case-sensitive) match first.
# - Then fall back to case-insensitive.
# - Accept abbreviations after . or _ or - (ie. f.b -> foo.bar).
# - Substring complete (ie. bar -> foobar).
zstyle ':completion:*' matcher-list '' '+m:{[:lower:]}={[:upper:]}' '+m:{[:upper:]}={[:lower:]}' '+m:{_-}={-_}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

# Colorize completions using default `ls` colors.
zstyle ':completion:*' list-colors ''

# Allow completion of ..<Tab> to ../ and beyond.
zstyle -e ':completion:*' special-dirs '[[ $PREFIX = (../)#(..) ]] && reply=(..)'

# Categorize completion suggestions with headings:
zstyle ':completion:*' group-name ''
zstyle ':completion:*:descriptions' format %F{default}%B%{$'\e[3m'%}--- %d ---%{$'\e[23m'%}%b%f

# Enable keyboard navigation of completions in menu
# (not just tab/shift-tab but cursor keys as well):
zstyle ':completion:*' menu select

#
# Fzf
#

if command -v fzf &> /dev/null; then
    source <(fzf --zsh)
fi

#
# Prompt
#

if ! command -v starship &> /dev/null; then
    curl -sS https://starship.rs/install.sh | sh && eval "$(starship init zsh)"
else
    eval "$(starship init zsh)"
fi

#
# History
#

export HISTSIZE=10000
export HISTFILE="$ZDOTDIR/history"
export SAVEHIST=$HISTSIZE

#
# Options
#

setopt AUTO_CD                 # [default] .. is shortcut for cd .. (etc)
setopt AUTO_PUSHD              # [default] cd automatically pushes old dir onto dir stack
setopt HIST_IGNORE_DUPS        # don't save dupes to history
setopt HIST_IGNORE_SPACE       # [default] don't record commands starting with a space
setopt LIST_PACKED             # make completion lists more densely packed
setopt MENU_COMPLETE           # auto-insert first possible ambiguous completion
setopt SHARE_HISTORY           # share history across shells

#
# Bindings
#

# Use emacs keybindings
bindkey -e

# Use "cbt" capability ("back_tab", as per `man terminfo`), if we have it:
if tput cbt &> /dev/null; then
    bindkey "$(tput cbt)" reverse-menu-complete # make Shift-tab go to previous completion
fi

bindkey ' ' magic-space # do history expansion on space

# Replace standard history-incremental-search-{backward,forward} bindings.
# These are the same but permit patterns (eg. a*b) to be used.
bindkey "^r" history-incremental-pattern-search-backward
bindkey "^s" history-incremental-pattern-search-forward

#
# Aliases
#

source $ZDOTDIR/aliases

#
# Plugins
#

source $ZDOTDIR/zsh-autosuggestions/zsh-autosuggestions.zsh
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=59'
