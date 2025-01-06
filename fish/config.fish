# Don't show a greeting
set -U fish_greeting

# Use the default key bindings
fish_default_key_bindings

# Environment
set -x LANG 'en_US.UTF-8'
set -x LC_ALL 'en_US.UTF-8'
set -x XDG_DATA_HOME {$HOME}/.local/share
set -x GPG_TTY (tty)

# Keep my secret configuration files in here.
if test -f $HOME/.config/fish/secrets.fish
    . $HOME/.config/fish/secrets.fish
end

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
set -x EDITOR nvim
set -x VISUAL "$EDITOR"
set -x ALTERNATE_EDITOR vim

# Zed editor
if test "$TERM_PROGRAM" = zed
    set -x EDITOR zed
    set -x VISUAL zed --wait
end

# Add to path if they exist
fish_add_path -aP /bin
fish_add_path -aP /usr/bin
fish_add_path -aP /usr/local/bin

# Local Paths
fish_add_path -aP "$HOME/.local/bin"
fish_add_path -aP "$HOME/.fly/bin"
fish_add_path -aP "$HOME/.local/share/racket/bin"

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
abbr gu gitu

# Helper functions
function mkcd
    mkdir -p $argv[1]; and cd $argv[1]
end

# Editing
fish_add_path -aP /opt/nvim/bin
abbr e nvim
abbr vim nvim
abbr se sudoedit
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
if command -q bat
    abbr cat bat
    set -x BAT_THEME ansi
    set -x BAT_PAGER
    set -x COLORTERM truecolor
end

# Use Hydro as our prompt
set -g hydro_symbol_prompt λ
set -g hydro_multiline true
set -g hydro_color_prompt $fish_color_normal
set -g hydro_color_git $fish_color_end

# Erlang and Elixir
fish_add_path -aP "$HOME/.mix/escripts"

set -x ERL_AFLAGS "-kernel shell_history enabled"
set -x KERL_BUILD_DOCS yes
set -x KERL_CONFIGURE_OPTIONS "--disable-debug --without-javac --without-wx"

abbr miex 'iex -S mix'

# PostgreSQL -- don't go to the users database which never exists...
set -x PGDATABASE postgres

# Zig
if test -d $HOME/.zvm
    set -x ZVM_INSTALL "$HOME/.zvm/self"
    fish_add_path -aP "$HOME/.zvm/bin"
    fish_add_path -aP $ZVM_INSTALL
end

# Lua
fish_add_path -aP "$HOME/.luarocks/bin"

# Go
fish_add_path -aP /usr/local/go/bin
if test -d $PROJECT_DIR/Go
    set -x GOPATH "$PROJECT_DIR/Go"
    fish_add_path -aP "$GOPATH/bin"
end

# OCaml configuration
if test -r "$HOME/.opam/opam-init/init.fish"
    source "$HOME/.opam/opam-init/init.fish"
end

# NodeJS
abbr pp pnpm
set -x NPM_PACKAGES "$HOME/.npm-packages"
set -x NODE_PATH "$NPM_PACKAGES/lib/node_modules:$NODE_PATH"
fish_add_path -aP "$NPM_PACKAGES/bin"

set -gx PNPM_HOME "$HOME/.local/share/pnpm"
if not string match -q -- $PNPM_HOME $PATH
    set -gx PATH "$PNPM_HOME" $PATH
end

# Deno
fish_add_path -aP "$HOME/.deno/bin"

# bun
set -x BUN_INSTALL "$HOME/.bun"
fish_add_path -aP "$BUN_INSTALL/bin"

# Common Lisp
abbr lisp 'rlwrap sbcl'

# Java
fish_add_path -aP "$HOME/.local/share/maven/bin"

# AWS
fish_add_path -aP "$HOME/.aws/bin"
set -x AWS_IAM_HOME "$HOME/.aws/iam"
set -x AWS_CREDENTIALS_FILE "$HOME/.aws/credentials"

# Docker
fish_add_path -aP $HOME/.docker/cli-plugins

# VTerm in Emacs
if [ "$INSIDE_EMACS" = vterm ]
    . $HOME/.config/fish/vterm.fish
end

# Mise for tool versioning
if command -q mise
    $HOME/.local/bin/mise activate fish | source
end

# FZF
# Generated here: https://vitormv.github.io/fzf-themes#eyJib3JkZXJTdHlsZSI6InJvdW5kZWQiLCJib3JkZXJMYWJlbCI6IiIsImJvcmRlckxhYmVsUG9zaXRpb24iOjAsInByZXZpZXdCb3JkZXJTdHlsZSI6InJvdW5kZWQiLCJwYWRkaW5nIjoiIiwibWFyZ2luIjoiMSIsInByb21wdCI6Is67ICIsIm1hcmtlciI6Ij4iLCJwb2ludGVyIjoi4peGIiwic2VwYXJhdG9yIjoi4pSAIiwic2Nyb2xsYmFyIjoi4pSCIiwibGF5b3V0IjoicmV2ZXJzZSIsImluZm8iOiJyaWdodCIsImNvbG9ycyI6ImZnKzojZDBkMGQwLGJnKzojMTYxNjFELGhsOiM2QTk1ODksaGwrOiM2NTg1OTQsaW5mbzojRENEN0JBLG1hcmtlcjojOThCQjZDLHByb21wdDojRkY5RTNCLHNwaW5uZXI6IzkzOEFBOSxwb2ludGVyOiM5NTdGQjgsaGVhZGVyOiM3Njk0NkEsYm9yZGVyOiMyQTJBMzcsbGFiZWw6I0RDRDdCQSxxdWVyeTojZDlkOWQ5In0=
set -Ux FZF_DEFAULT_OPTS '
  --color=fg:-1,fg+:#d0d0d0,bg:-1,bg+:#16161D
  --color=hl:#6A9589,hl+:#658594,info:#DCD7BA,marker:#98BB6C
  --color=prompt:#FF9E3B,spinner:#938AA9,pointer:#957FB8,header:#76946A
  --color=border:#2A2A37,label:#DCD7BA,query:#d9d9d9
  --border="rounded" --preview-window="border-rounded"
  --margin=1 --prompt="λ " --marker=">" --pointer="◆"
  --separator="─" --scrollbar="│" --layout="reverse" --info="right"'

# zoxide, a smarter cd
if command -q zoxide
    zoxide init fish | source
end

# atuin
if command -q atuin
    fish_add_path -aP $HOME/.atuin/bin
    atuin init fish | source
end

# bun
set --export BUN_INSTALL "$HOME/.bun"
set --export PATH $BUN_INSTALL/bin $PATH

# direnv
if command -q direnv
    eval (direnv hook fish)
end

