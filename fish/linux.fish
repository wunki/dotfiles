# Where I store all my code
set -x PROJECT_DIR "$HOME/code"

# Environment variables
set -x SHELL /usr/bin/fish

# Lua language server
fish_add_path -aP "$HOME/.local/share/lua-language-server/bin"

# Go
fish_add_path -aP /usr/local/go/bin
if type -q go
    set -x GOPATH "$PROJECT_DIR/go"
    fish_add_path -aP "$GOPATH/bin"
end

# Configuration specific to WSL2 Linux
if string match -q "*microsoft*" (uname -a)
    set -x GTK_THEME "Adwaita:dark"
    set -x BROWSER wslview
    set -x SHELL fish

    fish_add_path -aP "/mnt/c/Program Files/Docker/Docker/resources/bin"
    fish_add_path -aP "/mnt/c/Users/petar/AppData/Local/Programs/Microsoft VS Code/bin"
    fish_add_path -aP /mnt/c/Windows/System32

    abbr clip 'clip.exe'
end
