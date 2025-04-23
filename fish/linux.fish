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

# Java installation through Adoptium
set -l java_version "19.0.2+7"
if test -d "/opt/jdk-$java_version"
    set -x JAVA_HOME "/opt/jdk-$java_version"
    fish_add_path -aP "$JAVA_HOME/bin"
end

# Configuration specific to WSL2 Linux
if string match -q "*microsoft*" (uname -a)
    set -x PYTHON_KEYRING_BACKEND keyring.backends.null.Keyring
    set -x GTK_THEME "Adwaita:dark"
    set -x BROWSER wslview
    set -x SHELL fish

    # Setup SSH key agent
    setup-ssh-agent

    # Run Syncthing on startup. It won't fire if syncthing is already running
    # ~/.local/bin/start-syncing

    fish_add_path -aP "/mnt/c/Program Files/Docker/Docker/resources/bin"
    fish_add_path -aP "/mnt/c/Users/petar/AppData/Local/Programs/Microsoft VS Code/bin"
    fish_add_path -aP /mnt/c/Windows/System32

    abbr clip 'clip.exe'
end
