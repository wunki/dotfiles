# Don't show a greeting
set -U fish_greeting

# Environment
set -x LANG 'en_US.UTF-8'
set -x LC_ALL 'en_US.UTF-8'
set -x XDG_DATA_HOME {$HOME}/.local/share
set -x GPG_TTY (tty)

# Where I store all my projects
set -x PROJECT_DIR "$HOME/code"

if type -q nvim
    set -x EDITOR nvim
    set -x VISUAL nvim
else
    set -x EDITOR vim
    set -x VISUAL vim
end

# Add to path if they exist
fish_add_path -aP /bin
fish_add_path -aP /sbin
fish_add_path -aP /usr/bin
fish_add_path -aP /usr/sbin
fish_add_path -aP /usr/local/sbin
fish_add_path -aP /usr/local/bin
fish_add_path -aP /usr/local/share/dotnet
fish_add_path -aP (brew --prefix)/opt/node@14/bin

# Local Paths
fish_add_path -aP "$HOME/.local/bin"
fish_add_path -aP "$HOME/.npm/bin"
fish_add_path -aP "$HOME/.npm-packages/bin"
fish_add_path -aP "$HOME/.yarn/bin"
fish_add_path -aP "$HOME/.pyenv/bin"
fish_add_path -aP "$HOME/.fzf/bin"
fish_add_path -aP "$HOME/.fly/bin"

# Rust
# This fixes u bug where we weren't able to install components on FreeBSD
set -x RUSTUP_UPDATE_ROOT https://dev-static.rust-lang.org/rustup
fish_add_path -aP "$HOME/.cargo/bin"
abbr rfmt 'cargo +nightly fmt'

# Zig
fish_add_path -aP "$PROJECT_DIR/zig"

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
abbr n 'nvim'
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
set -x ERL_AFLAGS "-kernel shell_history enabled"
set -x KERL_CONFIGURE_OPTIONS "--disable-debug --without-javac"
abbr miex 'iex -S mix'
abbr mtm 'mix test --only module:'
fish_add_path -aP /usr/local/lib/erlang24/bin

# Python
if type -q pyenv
    status is-login; and pyenv init --path | source
    status is-interactive; and pyenv init - | source
    status --is-interactive; and pyenv virtualenv-init - | source
end

# PostgreSQL -- don't go to the users database which never exists...
fish_add_path -aP /usr/local/opt/libpq/bin
fish_add_path -aP /Applications/Postgres.app/Contents/Versions/latest/bin
type -q pgcli ; and set -x PGDATABASE postgres

# Use nvim when installed
if type -q nvim
    abbr v nvim
    abbr vim nvim
else
    abbr v vim
end

# FZF
type -q fzf ; and set fzf_preview_dir_cmd exa --all --color=always

# Go
if type -q go
    set -x GOPATH "$PROJECT_DIR/go"
    fish_add_path -aP "$GOPATH/bin"
    abbr gb 'go build'
    abbr gt 'go test -v ./...'

    type -q gocov ; and abbr gc 'gocov test | gocov report'
end

# NodeJS
if type -q npm
    set -x NPM_PACKAGES "$HOME/.npm-packages"
    set -x NODE_PATH "$NPM_PACKAGES/lib/node_modules:$NODE_PATH"
    fish_add_path -aP "$NPM_PACKAGES/bin"
end

# .NET Core
if type -q dotnet
    set -x DOTNET_CLI_TELEMETRY_OPTOUT true
    set -x DOTNET_SKIP_FIRST_TIME_EXPERIENCE true
end

# AWS 
fish_add_path -aP "$HOME/.aws/bin"
set -x AWS_IAM_HOME "$HOME/.aws/iam"
set -x AWS_CREDENTIALS_FILE "$HOME/.aws/credentials"

# Direnv: adding environment variables per directory in a .envrc
if type -q direnv
    eval (direnv hook fish)
end

# Version manager for different languages
test -f "$HOME/.asdf/asdf.fish" ; and source $HOME/.asdf/asdf.fish
test -d (brew --prefix)"/opt/asdf" ; and source (brew --prefix)/opt/asdf/libexec/asdf.fish

if type -q asdf
    fish_add_path -aP (asdf where elixir)/.mix/escripts
end

if status --is-interactive
  eval (/opt/homebrew/bin/brew shellenv)
end
