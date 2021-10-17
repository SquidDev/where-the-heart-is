#!/usr/bin/env sh

set -ex

sync() {
    cd "$1"
    pre-commit run --all > /dev/null || true # Run pre-commit just to make sure everything is formatted
    git-sync
}

{{#each git_sync}}
if ! (sync {{this}}); then
    notify-send --urgency=critical "Syncing failed for {{this}}"
fi
{{/each}}
