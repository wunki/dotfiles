# set -x TERM "xterm-kitty"
# abbr ssh 'env TERM=xterm-256color ssh'

# Configuration specific to WSL2 Linux
if string match -q "*microsoft*" (uname -a)
    set -x PYTHON_KEYRING_BACKEND keyring.backends.null.Keyring
    
    # keychain --eval --quiet --agents ssh id_rsa | source

    fish_add_path -aP "/mnt/c/Program Files/Docker/Docker/resources/bin"
    fish_add_path -aP "/mnt/c/Users/petar/AppData/Local/Programs/Microsoft VS Code/bin"
    fish_add_path -aP "/mnt/c/Windows/System32"

    abbr clip 'clip.exe'
end
