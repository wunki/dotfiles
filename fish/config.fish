# Don't show a greeting
set -U fish_greeting

# Environment
set -x LANG 'en_US.UTF-8'
set -x LC_ALL 'en_US.UTF-8'
set -x XDG_DATA_HOME {$HOME}/.local/share
set -x GPG_TTY (tty)

# System specific configuration
switch (uname)
    case Linux
        . $HOME/.config/fish/linux.fish
    case Darwin
        . $HOME/.config/fish/darwin.fish
    case FreeBSD
        . $HOME/.config/fish/freebsd.fish
end

# Where I store all my projects
set -x PROJECT_DIR "$HOME/Code"

# Shell variables
set -x SHELL fish
set -x EDITOR "hx"
set -x VISUAL "$EDITOR"
set -x ALTERNATE_EDITOR "vim"

# Add to path if they exist
fish_add_path -aP /bin
fish_add_path -aP /usr/bin
fish_add_path -aP /usr/local/bin

# Local Paths
fish_add_path -aP "$HOME/.local/bin"
fish_add_path -aP "$HOME/.pyenv/bin"
fish_add_path -aP "$HOME/.fly/bin"

# Configure the tide prompt
set --global tide_character_icon λ
set --global tide_character_color_failure "fb7da7"
set --global tide_character_color "76c5a4"
set --global tide_pwd_color_anchors "8bb8d0"
set --global tide_pwd_color_dirs "5385af"

# Rust
fish_add_path -aP "$HOME/.cargo/bin"
abbr rfmt 'cargo +nightly fmt'
if type -q sccache
    set -x RUSTC_WRAPPER (which sccache)
end

# Tree
abbr t1 'tree --dirsfirst -ChFL 1'
abbr t2 'tree --dirsfirst -ChFL 2'
abbr t3 'tree --dirsfirst -ChFL 3'

# Git shortcuts
abbr gc 'git commit'
abbr gs 'git status --ignore-submodules=dirty'
abbr gp 'git push origin HEAD'
abbr gf 'git pull origin HEAD'

# Editing
abbr e 'hx'
abbr se 'sudoedit'
abbr cdr 'cd (git rev-parse --show-toplevel)'

# Tmux
abbr tt 'tmux new-session -A -s main'

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

fish_add_path -aP /usr/local/lib/erlang25/bin
fish_add_path -aP "$HOME/.local/share/elixir-ls/release"

# PostgreSQL -- don't go to the users database which never exists...
fish_add_path -aP /usr/local/opt/libpq/bin
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
set -gx FZF_DEFAULT_OPTS "
--multi
--height=50%
--prompt='❯ '
--pointer='-'
--marker='+'
--ansi
--tabstop=4
--color=dark
--color=hl:2:bold,fg+:4:bold,bg+:-1,hl+:2:bold,info:3:bold,border:0,prompt:2,pointer:5,marker:1,header:6
"

# Go
if type -q go
    set -x GOPATH "$PROJECT_DIR/go"
    fish_add_path -aP "$GOPATH/bin"
end

# NodeJS
if type -q npm
    set -x NPM_PACKAGES "$HOME/.npm-packages"
    set -x NODE_PATH "$NPM_PACKAGES/lib/node_modules:$NODE_PATH"
    fish_add_path -aP "$NPM_PACKAGES/bin"
end

# AWS 
fish_add_path -aP "$HOME/.aws/bin"
set -x AWS_IAM_HOME "$HOME/.aws/iam"
set -x AWS_CREDENTIALS_FILE "$HOME/.aws/credentials"

# Direnv: adding environment variables per directory in a .envrc
if type -q direnv
    eval (direnv hook fish)
end

# Docker
fish_add_path -aP $HOME/.docker/cli-plugins

# ASDF: version manager for different languages
test -f "$HOME/.asdf/asdf.fish" ; and source $HOME/.asdf/asdf.fish
