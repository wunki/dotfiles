abbr paco 'sudo pacman -Rs (pacman -Qqtd)'
abbr ports 'sudo lsof -PiTCP -sTCP:LISTEN'


# Configuration specific to WSL2 Linux
if string match -q "*microsoft*" (uname -a)
    # set -x DISPLAY (cat /etc/resolv.conf | grep nameserver | awk '{print $2; exit;}'):0.0
    set -x GDK_SCALE 0.5
    set -x GDK_DPI_SCALE 2
    set -x PYTHON_KEYRING_BACKEND keyring.backends.null.Keyring
    keychain --eval --quiet --agents ssh id_rsa | source

    prepend_to_path "/mnt/c/Program Files/Docker/Docker/resources/bin"
    prepend_to_path "/mnt/c/Users/Petar Radosevic/AppData/Local/Programs/Microsoft VS Code/bin"
    prepend_to_path "/mnt/c/Windows/System32"

    abbr clip 'clip.exe'
end
