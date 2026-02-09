# Where I store all my code
set -x PROJECT_DIR "$HOME/Code"

# Setup environment
if test -x /opt/homebrew/bin/fish
    set -x SHELL /opt/homebrew/bin/fish
end

# Setup homebrew
set -U brew_prefix /opt/homebrew

# Don't show me hints
set -x HOMEBREW_NO_ENV_HINTS true

# Don't update so aggressively
set -x HOMEBREW_NO_AUTO_UPDATE 1
set -x HOMEBREW_NO_INSTALL_UPGRADE 1

# Mac specific paths
fish_add_path -aP "$brew_prefix/bin"

# Python
fish_add_path -pP "$brew_prefix/opt/python3/bin"

# Go
fish_add_path -aP /usr/local/go/bin
if type -q go
    set -x GOPATH "$PROJECT_DIR/Go"
    fish_add_path -aP "$GOPATH/bin"
end

# Emacs
fish_add_path -aP /Applications/Emacs.app/Contents/MacOS
fish_add_path -aP /Applications/Emacs.app/Contents/MacOS/bin

# Auto completion
if test -d "$brew_prefix/share/fish/completions"
    set -p fish_complete_path "$brew_prefix/share/fish/completions"
end

if test -d "$brew_prefix/share/fish/vendor_completions.d"
    set -p fish_complete_path "$brew_prefix/share/fish/vendor_completions.d"
end

# Setup Tailscale
alias tailscale "/Applications/Tailscale.app/Contents/MacOS/Tailscale"

# Ruby
fish_add_path -pP "$brew_prefix/opt/ruby/bin"
fish_add_path -pP "$brew_prefix/lib/ruby/gems/3.4.0/bin/"

# PostgreSQL
fish_add_path -aP "$brew_prefix/opt/libpq/bin"
fish_add_path -aP /Applications/Postgres.app/Contents/Versions/latest/bin

# SQLite
fish_add_path -pP "$brew_prefix/opt/sqlite3/bin"
set -gx LDFLAGS "-L$brew_prefix/opt/sqlite/lib"
set -gx CPPFLAGS "-I$brew_prefix/opt/sqlite/include"

# OpenSSL
set -x OPENSSL_INCLUDE_DIR "$brew_prefix/opt/openssl/include"
set -x OPENSSL_LIB "$brew_prefix/opt/openssl/lib"
set -x OPENSSL_ROOT_DIR "$brew_prefix/opt/openssl"

# If I'm in the ZED shell, set the editor to Zed
if set -q ZED_TERM
    set -x EDITOR 'zed --wait'
end

function bup --description "Updates, upgrades and cleans Homebrew"
    brew update
    brew upgrade
    brew cleanup
end

# Elixir: partition os_deps compile work by CPU cores / 2
set -x MIX_OS_DEPS_COMPILE_PARTITION_COUNT (math --scale=0 (sysctl -n hw.physicalcpu) / 2)
