# don't show a greeting
set -U fish_greeting

# use the default key bindings
fish_default_key_bindings

# environment
set -x LANG 'en_US.UTF-8'
set -x LC_ALL 'en_US.UTF-8'
set -x XDG_DATA_HOME {$HOME}/.local/share
set -x GPG_TTY (tty)

# keep my secret configuration files in here.
if test -f $HOME/.config/fish/secrets.fish
    . $HOME/.config/fish/secrets.fish
end

# system specific configuration
switch (uname)
    case Linux
        . $HOME/.config/fish/linux.fish
    case Darwin
        . $HOME/.config/fish/darwin.fish
    case FreeBSD
        . $HOME/.config/fish/freebsd.fish
end

# editor configuration
set -x EDITOR nvim
set -x VISUAL "$EDITOR"
set -x ALTERNATE_EDITOR vim

# zed
if test "$TERM_PROGRAM" = zed
    set -x EDITOR zed
    set -x VISUAL zed --wait
end

# system paths
fish_add_path -aP /bin
fish_add_path -aP /usr/bin
fish_add_path -aP /usr/local/bin

# local paths
fish_add_path -aP "$HOME/.local/bin"
fish_add_path -aP "$HOME/.fly/bin"
fish_add_path -aP "$HOME/.local/share/racket/bin"

# development tools
fish_add_path -aP "$HOME/.cargo/bin"
fish_add_path -aP /opt/nvim/bin

# abbreviations and aliases
abbr e $EDITOR
abbr vim nvim
abbr se sudoedit
abbr cdr 'cd (git rev-parse --show-toplevel)'

# tree shortcuts
abbr t1 'tree --dirsfirst -ChFL 1'
abbr t2 'tree --dirsfirst -ChFL 2'
abbr t3 'tree --dirsfirst -ChFL 3'

# git shortcuts
abbr gc 'git commit'
abbr gs 'git status --ignore-submodules=dirty'
abbr gp 'git push origin HEAD'
abbr gpf 'git push --force-with-lease origin HEAD'
abbr gf 'git pull origin HEAD'
abbr gu gitu
abbr gopen 'gh repo view --web'

# file listing (eza)
abbr l eza
abbr ls eza
abbr ll 'eza -l'
abbr llg 'eza -l --git'
abbr lll 'eza -la'

# tmux shortcuts
abbr tt 'tmux attach || tmux new-session -s main'
abbr ssb "ssh -t bytebottom 'tmux attach || tmux new-session -s main'"

# quality of life functions
function mkcd
    mkdir -p $argv[1]; and cd $argv[1]
end

# tool configurations
if type -q bat
    abbr cat bat
    set -x BAT_THEME ansi
    set -x BAT_PAGER
    set -x COLORTERM truecolor
end

# prompt (using starship)
if type -q starship
    starship init fish | source
end

# node.js ecosystem
set -x NPM_PACKAGES "$HOME/.npm-packages"
set -x NODE_PATH "$NPM_PACKAGES/lib/node_modules:$NODE_PATH"
fish_add_path -aP "$NPM_PACKAGES/bin"

# pnpm
set -x PNPM_HOME "$HOME/.local/share/pnpm"
fish_add_path -aP "$PNPM_HOME"
abbr pp pnpm
abbr ppx pnpmx

# bun
set -x BUN_INSTALL "$HOME/.bun"
fish_add_path -aP "$BUN_INSTALL/bin"

# elixir/erlang (beam)
fish_add_path -aP "$HOME/.mix/escripts"
set -x ERL_AFLAGS "-kernel shell_history enabled"
set -x KERL_BUILD_DOCS yes
set -x KERL_CONFIGURE_OPTIONS "--disable-debug --without-javac --without-wx"
abbr miex 'iex -S mix'

# postgresql
set -x PGDATABASE postgres

# other languages and tools
abbr lisp 'rlwrap sbcl'
fish_add_path -aP "$HOME/.local/share/lua-language-server/bin"

# cloud and devops
fish_add_path -aP "$HOME/.aws/bin"
set -x AWS_IAM_HOME "$HOME/.aws/iam"
set -x AWS_CREDENTIALS_FILE "$HOME/.aws/credentials"
fish_add_path -aP $HOME/.docker/cli-plugins

# development tools
fish_add_path -aP $HOME/.opencode/bin

# editor integrations
if [ "$INSIDE_EMACS" = vterm ]
    . $HOME/.config/fish/vterm.fish
end

# environment managers (with performance guards)
if type -q mise
    $HOME/.local/bin/mise activate fish | source
end

if type -q direnv
    direnv hook fish | source
end

# Added by OrbStack: command-line tools and integration
# This won't be added again if you remove it.
source ~/.orbstack/shell/init2.fish 2>/dev/null || :

# opencode
fish_add_path /Users/petar/.opencode/bin
