#!/bin/bash
#
# nvim-url-handler.sh - Handle nvim:// URLs to open files in running Neovim instances
#
# URL Format: nvim://file//path/to/file.txt:42?tmux-session=session-name
#
# This script is called by the AppleScript URL handler app.
# It looks for a running Neovim instance with a socket for the given tmux session,
# and if found, opens the file there. Otherwise, it opens a new Ghostty window.
#

TMUX_SESSION_NAME="$1"
FILE_PATH_WITH_LINE="$2"

# Homebrew paths (macOS)
NVIM="/opt/homebrew/bin/nvim"
GHOSTTY="/Applications/Ghostty.app/Contents/MacOS/ghostty"

# Parse file path and line number (format: /path/to/file.txt:42)
if [[ "$FILE_PATH_WITH_LINE" =~ ^(.+):([0-9]+)$ ]]; then
    FILE_PATH="${BASH_REMATCH[1]}"
    LINE_NUM="${BASH_REMATCH[2]}"
else
    FILE_PATH="$FILE_PATH_WITH_LINE"
    LINE_NUM=""
fi

# Build nvim arguments
if [ -n "$LINE_NUM" ]; then
    NVIM_ARGS="+$LINE_NUM"
else
    NVIM_ARGS=""
fi

focus_ghostty() {
    osascript -e 'tell application "Ghostty" to activate'
}

select_tmux_nvim_pane() {
    local session="$1"
    local tmux="/opt/homebrew/bin/tmux"

    # Switch client to the target session
    $tmux switch-client -t "$session" 2>/dev/null

    # Find pane running nvim in this session
    local nvim_pane=$($tmux list-panes -s -t "$session" \
        -F '#{window_index}:#{pane_index} #{pane_current_command}' 2>/dev/null \
        | grep -i nvim | head -1 | cut -d' ' -f1)

    if [ -n "$nvim_pane" ]; then
        local window="${nvim_pane%%:*}"
        local pane="${nvim_pane##*:}"
        $tmux select-window -t "$session:$window" 2>/dev/null
        $tmux select-pane -t "$session:$window.$pane" 2>/dev/null
    fi
}

if [ -n "$TMUX_SESSION_NAME" ] && [ -n "$FILE_PATH" ]; then
    SOCKET_PATH="/tmp/nvim-$TMUX_SESSION_NAME"

    if [ -e "$SOCKET_PATH" ]; then
        # Try to open in existing Neovim instance
        if [ -n "$LINE_NUM" ]; then
            $NVIM --server "$SOCKET_PATH" --remote-send "<Esc>:e +$LINE_NUM $FILE_PATH<CR>"
        else
            $NVIM --server "$SOCKET_PATH" --remote "$FILE_PATH"
        fi

        if [ $? -ne 0 ]; then
            # Fallback: open in new Ghostty window
            $GHOSTTY -e $NVIM $NVIM_ARGS "$FILE_PATH"
        else
            # Switch to the correct tmux window/pane
            select_tmux_nvim_pane "$TMUX_SESSION_NAME"
        fi
        focus_ghostty
    else
        # No socket found, open in new Ghostty window
        $GHOSTTY -e $NVIM $NVIM_ARGS "$FILE_PATH"
        focus_ghostty
    fi
elif [ -n "$FILE_PATH" ]; then
    # No session specified, just open the file
    $GHOSTTY -e $NVIM $NVIM_ARGS "$FILE_PATH"
    focus_ghostty
fi
