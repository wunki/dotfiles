#!/bin/bash
#
# Open nvim:// file URLs in the Neovim process for a tmux session.
#
# Format: nvim://file//path/to/file.txt:42?tmux-session=session-name
# The AppleScript URL handler calls this script. A missing socket falls back to
# a new Ghostty window.
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

    # Bring the target session and Neovim pane forward.
    $tmux switch-client -t "$session" 2>/dev/null

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
        # Prefer the Neovim process already serving this tmux session.
        if [ -n "$LINE_NUM" ]; then
            $NVIM --server "$SOCKET_PATH" --remote-send "<Esc>:e +$LINE_NUM $FILE_PATH<CR>"
        else
            $NVIM --server "$SOCKET_PATH" --remote "$FILE_PATH"
        fi

        if [ $? -ne 0 ]; then
            # The socket is stale or unreachable. Open a fresh terminal.
            $GHOSTTY -e $NVIM $NVIM_ARGS "$FILE_PATH"
        else
            select_tmux_nvim_pane "$TMUX_SESSION_NAME"
        fi
        focus_ghostty
    else
        # No process is listening for this session.
        $GHOSTTY -e $NVIM $NVIM_ARGS "$FILE_PATH"
        focus_ghostty
    fi
elif [ -n "$FILE_PATH" ]; then
    # Without a session, open the file in a new terminal.
    $GHOSTTY -e $NVIM $NVIM_ARGS "$FILE_PATH"
    focus_ghostty
fi
