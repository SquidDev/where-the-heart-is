#!/usr/bin/env sh

set -x

{{#if dotter.packages.tmux}}
if tmux info &> /dev/null; then
    tmux source-file ~/.config/tmux/tmux.conf
fi

tic -x -o ~/.terminfo/ ~/.config/tmux/tmux-24bit.terminfo
{{/if}}
