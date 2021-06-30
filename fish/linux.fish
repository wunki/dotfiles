# Configuration specific to WSL2 Linux
if string match -q "*microsoft*" (uname -a)
    set -x PYTHON_KEYRING_BACKEND keyring.backends.null.Keyring
    
    # keychain --eval --quiet --agents ssh id_rsa | source

    prepend_to_path "/mnt/c/Program Files/Docker/Docker/resources/bin"
    prepend_to_path "/mnt/c/Users/Petar Radosevic/AppData/Local/Programs/Microsoft VS Code/bin"
    prepend_to_path "/mnt/c/Windows/System32"

    abbr clip 'clip.exe'
end
