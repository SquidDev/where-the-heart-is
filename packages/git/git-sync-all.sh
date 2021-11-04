#!/usr/bin/env sh

set -ex

sync() {
    cd "$1"
    # Run pre-commit just to make sure everything is formatted
    git ls-files --others --exclude-standard --cached | xargs -r pre-commit run --files > /dev/null || true
    git-sync
}

{{#each git_sync}}
if ! (sync {{this}}); then
    notify-send --urgency=critical "Syncing failed for {{this}}"
fi
{{/each}}
