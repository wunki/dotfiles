# Don't show a greeting
set -U fish_greeting

# Use the default key bindings
fish_default_key_bindings

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
set -x PROJECT_DIR "$HOME/Developer"

# Shell variables
set -x SHELL fish
set -x EDITOR "nvim"
set -x VISUAL "$EDITOR"
set -x ALTERNATE_EDITOR "vim"

# Add to path if they exist
fish_add_path -aP /bin
fish_add_path -aP /usr/bin
fish_add_path -aP /usr/local/bin

# Local Paths
fish_add_path -aP "$HOME/.local/bin"
fish_add_path -aP "$HOME/.fly/bin"
fish_add_path -aP "$HOME/.local/share/racket/bin"

# Configure the tide prompt with oh-lucy colors
set --global tide_character_icon "🐟"
set --global tide_character_color_failure "FB7DA7"
set --global tide_character_color "76C5A4"
set --global tide_pwd_color_anchors "8BB8D0"
set --global tide_pwd_color_dirs "5385AF"
set --global tide_git_color_branch "7EC49D"

# Rust
fish_add_path -aP "$HOME/.cargo/bin"
abbr rfmt 'cargo +nightly fmt'

# Tree
abbr t1 'tree --dirsfirst -ChFL 1'
abbr t2 'tree --dirsfirst -ChFL 2'
abbr t3 'tree --dirsfirst -ChFL 3'

# Git shortcuts
abbr gc 'git commit'
abbr gs 'git status --ignore-submodules=dirty'
abbr gp 'git push origin HEAD'
abbr gf 'git pull origin HEAD'
abbr lg 'lazygit'

# Editing
fish_add_path -aP /opt/nvim/bin
abbr e 'nvim'
abbr se 'sudoedit'
abbr cdr 'cd (git rev-parse --show-toplevel)'

# Tmux
abbr tt 'tmux attach || tmux new-session -s main'

# Use eza for listing files
abbr l eza
abbr ls eza
abbr ll 'eza -l'
abbr llg 'eza -l --git'
abbr lll 'eza -la'

# Use bat because of syntax highlighting
if type -q bat
    abbr cat bat
    set -x BAT_THEME ansi
    set -x BAT_PAGER
    set -x COLORTERM truecolor
end

# Use Hydro as our prompt.
set -g hydro_symbol_prompt λ
set -g hydro_multiline true
set -g hydro_color_prompt "#ffa066"

# Erlang and Elixir
set -x ERL_AFLAGS "-kernel shell_history enabled"
set -x KERL_BUILD_DOCS "yes"
set -x KERL_CONFIGURE_OPTIONS "--disable-debug --without-javac --without-wx"

fish_add_path -aP "$HOME/.mix/escripts"

abbr miex 'iex -S mix'

# PostgreSQL -- don't go to the users database which never exists...
set -x PGDATABASE postgres

# Use neovim
abbr e nvim

# Zig
if test -d $HOME/.zvm
    set -x ZVM_INSTALL "$HOME/.zvm/self"
    fish_add_path -aP "$HOME/.zvm/bin"
    fish_add_path -aP $ZVM_INSTALL
end

# Lua
fish_add_path -aP "$HOME/.luarocks/bin"

# Go
if test -d $PROJECT_DIR/Go
    set -x GOPATH "$PROJECT_DIR/Go"
    fish_add_path -aP "$GOPATH/bin"
end

# NodeJS
set -x NPM_PACKAGES "$HOME/.npm-packages"
set -x NODE_PATH "$NPM_PACKAGES/lib/node_modules:$NODE_PATH"
fish_add_path -aP "$NPM_PACKAGES/bin"

# Common Lisp
abbr lisp 'rlwrap sbcl'

# Java
fish_add_path -aP "$HOME/.local/share/maven/bin"

# AWS
fish_add_path -aP "$HOME/.aws/bin"
set -x AWS_IAM_HOME "$HOME/.aws/iam"
set -x AWS_CREDENTIALS_FILE "$HOME/.aws/credentials"

# Direnv: adding environment variables per directory in a .envrc
# eval (direnv hook fish)

# Docker
fish_add_path -aP $HOME/.docker/cli-plugins

# VTerm in Emacs
if [ "$INSIDE_EMACS" = 'vterm' ]
    . $HOME/.config/fish/vterm.fish
end

# Trying out Rye for managing Python
fish_add_path -aP $HOME/.rye/shims
