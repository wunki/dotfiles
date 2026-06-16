# Where I store all my code
set -x PROJECT_DIR "$HOME/Code"

# Environment variables
if test -x /usr/bin/fish
    set -x SHELL /usr/bin/fish
else if type -q fish
    set -x SHELL (command -s fish)
end

# Auto completion
for completions_dir in \
    /usr/local/share/fish/completions \
    /usr/local/share/fish/vendor_completions.d \
    /usr/share/fish/completions \
    /usr/share/fish/vendor_completions.d
    if test -d "$completions_dir"; and not contains -- "$completions_dir" $fish_complete_path
        set -p fish_complete_path "$completions_dir"
    end
end

# Clipboard
if type -q wl-copy
    abbr cpwd 'pwd | wl-copy'
else if type -q xclip
    abbr cpwd 'pwd | xclip -selection clipboard'
else if type -q xsel
    abbr cpwd 'pwd | xsel --clipboard --input'
end

# Lua language server
fish_add_path -aP "$HOME/.local/share/lua-language-server/bin"

# Ruby
for ruby_gem_bin in "$HOME"/.local/share/gem/ruby/*/bin
    if test -d "$ruby_gem_bin"
        fish_add_path -aP "$ruby_gem_bin"
    end
end

# Go
fish_add_path -aP /usr/local/go/bin
if type -q go
    set -x GOPATH "$PROJECT_DIR/go"
    fish_add_path -aP "$GOPATH/bin"
end

# PostgreSQL
fish_add_path -aP /usr/lib/postgresql/*/bin

# SQLite
if test -f /usr/include/sqlite3.h
    set -gx CPPFLAGS "-I/usr/include $CPPFLAGS"
end

# OpenSSL
if test -d /usr/include/openssl
    set -x OPENSSL_INCLUDE_DIR /usr/include
end

if test -d /usr/lib/ssl
    set -x OPENSSL_ROOT_DIR /usr
end

for openssl_lib_dir in /usr/lib /usr/lib64 /usr/lib/*-linux-gnu
    if test -f "$openssl_lib_dir/libssl.so"
        set -x OPENSSL_LIB_DIR "$openssl_lib_dir"
        break
    end
end

# Elixir: partition os_deps compile work by CPU cores / 2
if type -q nproc
    set -l cpu_count (nproc 2>/dev/null)
    if test $status -eq 0; and string match -qr '^[0-9]+$' -- $cpu_count
        set -l os_deps_partition_count (math --scale=0 "$cpu_count / 2")
        if test $os_deps_partition_count -lt 1
            set os_deps_partition_count 1
        end

        set -x MIX_OS_DEPS_COMPILE_PARTITION_COUNT $os_deps_partition_count
    end
end

# Configuration specific to WSL2 Linux
if string match -q "*microsoft*" (uname -a)
    set -x GTK_THEME "Adwaita:dark"
    set -x BROWSER "/mnt/c/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"

    fish_add_path -aP "/mnt/c/Program Files/Docker/Docker/resources/bin"
    fish_add_path -aP "/mnt/c/Users/petar/AppData/Local/Programs/Microsoft VS Code/bin"
    fish_add_path -aP /mnt/c/Windows/System32

    abbr clip 'clip.exe'
end
