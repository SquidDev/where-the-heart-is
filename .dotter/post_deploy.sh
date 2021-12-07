#!/usr/bin/env sh

set -x

{{#if dotter.packages.albert}}
albert restart
{{/if}}

{{#if dotter.packages.emacs}}
update-desktop-database ~/.local/share/applications
xdg-mime default emacsclient.desktop x-scheme-handler/org-protocol
{{/if}}

{{#if dotter.packages.fcitx5}}
fcitx5-remote -r
{{/if}}

{{#if dotter.packages.git}}
systemctl daemon-reload --user
systemctl enable --now --user git-sync.timer
{{/if}}

{{#if dotter.packages.tmux}}
if [ ! -d ~/.config/tmux/plugins/tpm ]; then
    mkdir -p ~/.config/tmux/plugins
    git clone https://github.com/tmux-plugins/tpm ~/.config/tmux/plugins/tpm
fi

if tmux info &> /dev/null; then
    tmux source-file ~/.config/tmux/tmux.conf
fi

tic -x -o ~/.terminfo/ ~/.config/tmux/tmux-24bit.terminfo
{{/if}}
