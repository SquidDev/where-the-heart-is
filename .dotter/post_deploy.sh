#!/usr/bin/env sh

set -x

{{#if dotter.packages.albert}}
albert restart
{{/if}}

{{#if dotter.packages.emacs}}
update-desktop-database ~/.local/share/applications
xdg-mime default emacsclient.desktop x-scheme-handler/org-protocol
{{/if}}

{{#if dotter.packages.tmux}}
if tmux info &> /dev/null; then
    tmux source-file ~/.config/tmux/tmux.conf
fi

tic -x -o ~/.terminfo/ ~/.config/tmux/tmux-24bit.terminfo
{{/if}}
