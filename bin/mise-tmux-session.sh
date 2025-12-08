#!/usr/bin/env bash
# Export current tmux session name for use in mise templates

if [ -n "$TMUX" ]; then
    export TMUX_SESSION_NAME=$(tmux display-message -p '#S')
else
    export TMUX_SESSION_NAME=""
fi
