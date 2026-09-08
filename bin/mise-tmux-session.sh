#!/usr/bin/env bash
# Export the current tmux session name for use in mise templates.

TMUX_SESSION_NAME=""
if [ -n "$TMUX" ]; then
    TMUX_SESSION_NAME="$(tmux display-message -p '#S')"
fi
export TMUX_SESSION_NAME
