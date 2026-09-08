# Where I store all my code
set -x PROJECT_DIR "$HOME/Code"

# Login shell
if test -x /opt/homebrew/bin/fish
    set -x SHELL /opt/homebrew/bin/fish
end

# Homebrew
if test -x /opt/homebrew/bin/brew
    set -g brew_prefix /opt/homebrew
else if test -x /usr/local/bin/brew
    set -g brew_prefix /usr/local
else
    set -g brew_prefix /opt/homebrew
end

# Don't show me hints
set -x HOMEBREW_NO_ENV_HINTS true

# Leave updates to the explicit `bup` command.
set -x HOMEBREW_NO_AUTO_UPDATE 1
set -x HOMEBREW_NO_INSTALL_UPGRADE 1

# macOS paths
fish_add_path -aP "$brew_prefix/bin"

abbr cpwd 'pwd | pbcopy'

# Added by OrbStack: command-line tools and integration
if test -f "$HOME/.orbstack/shell/init2.fish"
    source "$HOME/.orbstack/shell/init2.fish"
end

# Python
fish_add_path -pP "$brew_prefix/opt/python3/bin"

# Go
fish_add_path -aP /usr/local/go/bin
if type -q go
    set -x GOPATH "$PROJECT_DIR/go"
    fish_add_path -aP "$GOPATH/bin"
end

# Completions
for completions_dir in \
    "$brew_prefix/share/fish/completions" \
    "$brew_prefix/share/fish/vendor_completions.d"
    if test -d "$completions_dir"; and not contains -- "$completions_dir" $fish_complete_path
        set -p fish_complete_path "$completions_dir"
    end
end

# Tailscale
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

function bup --description "Updates, upgrades and cleans Homebrew"
    brew update
    brew upgrade
    brew cleanup
end
