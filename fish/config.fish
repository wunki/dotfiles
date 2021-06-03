# Don't show a greeting
set -U fish_greeting

# Environment
set -x LANG 'en_US.UTF-8'
set -x LC_ALL 'en_US.UTF-8'
set -x XDG_DATA_HOME {$HOME}/.local/share
set -x GPG_TTY (tty)

if type -q nvim
    set -x EDITOR nvim
    set -x VISUAL nvim
else
    set -x EDITOR vim
    set -x VISUAL vim
end

# Start with a clean path variable
set -e PATH
prepend_to_path /bin
prepend_to_path /sbin
prepend_to_path /usr/bin
prepend_to_path /usr/sbin
prepend_to_path /usr/local/sbin
prepend_to_path /usr/local/bin
prepend_to_path /usr/local/share/dotnet

# Local Paths
prepend_to_path "$HOME/.bin"
prepend_to_path "$HOME/.local/bin"
prepend_to_path "$HOME/.npm/bin"
prepend_to_path "$HOME/.npm-packages/bin"
prepend_to_path "$HOME/.yarn/bin"

# Rust
# This fixes u bug where we weren't able to install components on FreeBSD
set -x RUSTUP_UPDATE_ROOT https://dev-static.rust-lang.org/rustup
prepend_to_path "$HOME/.cargo/bin"

# Zig
prepend_to_path "$PROJECT_DIR/zig"

# System specific configuration
switch (uname)
    case Linux
        . $HOME/.config/fish/linux.fish
    case Darwin
        . $HOME/.config/fish/darwin.fish
    case FreeBSD
        . $HOME/.config/fish/freebsd.fish
end

abbr t1 'tree --dirsfirst -ChFL 1'
abbr t2 'tree --dirsfirst -ChFL 2'
abbr t3 'tree --dirsfirst -ChFL 3'
abbr gs 'git status --ignore-submodules=dirty'
abbr gp 'git push origin HEAD'
abbr gf 'git pull origin HEAD'
abbr e 'emacsclient --no-wait --create-frame --quiet -a emacs'
abbr cdr 'cd (git rev-parse --show-toplevel)'

# Use EXA for listing files
if type -q exa
    abbr l exa
    abbr ls exa
    abbr ll 'exa -l'
    abbr llg 'exa -l --git'
    abbr lll 'exa -la'
else
    abbr l ls
    abbr ll 'ls -l'
    abbr lll 'ls -la'
end

# Use bat because of syntax highlighting
if type -q bat
    abbr cat bat
    set -x BAT_THEME base16
    set -x COLORTERM truecolor
end

# Erlang and Elixir
abbr miex 'iex -S mix'
abbr mtm 'mix test --only module:'
prepend_to_path /usr/local/lib/erlang23/bin

if type -q erl
    set -x ERL_AFLAGS "-kernel shell_history enabled"
end

# PostgreSQL -- don't go to the users database which never exists...
if type -q pgcli
    set -x PGDATABASE postgres
end

# Use nvim when installed
if type -q nvim
    abbr v nvim
    abbr vim nvim
else
    abbr v vim
end

# FZF
if type -q fzf
    set fzf_preview_dir_cmd exa --all --color=always
end

# Go
if type -q go
    set -x GOPATH "$PROJECT_DIR/go"
    prepend_to_path "$GOPATH/bin"
    abbr gb 'go build'
    abbr gt 'go test -v ./...'

    if type -q gocov
        abbr gc 'gocov test | gocov report'
    end
end

# NodeJS
if type -q npm
    set -x NPM_PACKAGES "$HOME/.npm-packages"
    set -x NODE_PATH "$NPM_PACKAGES/lib/node_modules:$NODE_PATH"
    prepend_to_path "$NPM_PACKAGES/bin"
end

# .NET Core
if type -q dotnet
    set -x DOTNET_CLI_TELEMETRY_OPTOUT true
    set -x DOTNET_SKIP_FIRST_TIME_EXPERIENCE true
end

# AWS 
prepend_to_path "$HOME/.aws/bin"
set -x AWS_IAM_HOME "$HOME/.aws/iam"
set -x AWS_CREDENTIALS_FILE "$HOME/.aws/credentials"

# Direnv: adding environment variables per directory in a .envrc
if type -q direnv
    eval (direnv hook fish)
end

# Version manager for different languages
if test -d "$HOME/.asdf"
    . $HOME/.asdf/asdf.fish
end
