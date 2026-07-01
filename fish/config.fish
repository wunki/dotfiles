# don't show a greeting
set -g fish_greeting

# environment
set -x LANG 'en_US.UTF-8'
set -x LC_ALL 'en_US.UTF-8'
set -x XDG_DATA_HOME "$HOME/.local/share"
if status is-interactive
    set -l gpg_tty (tty 2>/dev/null)
    if test $status -eq 0; and test -n "$gpg_tty"
        set -x GPG_TTY $gpg_tty
    end
end

# keep my secret configuration files in here.
if test -f $HOME/.config/fish/secrets.fish
    . $HOME/.config/fish/secrets.fish
end

# system specific configuration
set -l fish_build_target (status buildinfo 2>/dev/null | string match --regex --groups-only '^Target.*: (.*)$')
if test -z "$fish_build_target"
    set fish_build_target (uname)
end
switch $fish_build_target
    case '*linux*' Linux
        . $HOME/.config/fish/linux.fish
    case '*darwin*' Darwin
        . $HOME/.config/fish/darwin.fish
    case '*freebsd*' FreeBSD
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
function cdr --description "Change to the current git repository root"
    set -l root (git rev-parse --show-toplevel 2>/dev/null); or begin
        echo "cdr: not inside a git repository" >&2
        return 1
    end

    cd -- $root
end

# herdr stuff
abbr hrd herdr --remote desktop

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
abbr gho 'gh browse'

# file listing (eza)
abbr l eza
abbr ls eza
abbr ll eza
abbr llg 'eza -l --git'
abbr lll 'eza -la'

# tmux shortcuts
abbr tt 'tmux attach || tmux new-session -s main'
abbr ssb "ssh -t bytebottom 'tmux attach || tmux new-session -s main'"

# autossh
abbr ash 'autossh -M 0 -q'

# quality of life functions
function mkcd
    if test (count $argv) -eq 0
        echo "mkcd: missing directory"
        return 1
    end

    mkdir -p -- $argv[1]; and cd -- $argv[1]
end

# tmux-aware nvim socket for nvim:// links
function nvs --description "Start nvim with tmux session socket"
    if not set -q TMUX
        echo "nvs: not in a tmux session, use nvim instead"
        return 1
    end

    set -l session_name (tmux display-message -p '#S')
    set -l socket_path "/tmp/nvim-$session_name"

    if test -e "$socket_path"
        if not nvim --server "$socket_path" --remote-expr 1 >/dev/null 2>&1
            rm -f "$socket_path"
        end
    end

    nvim --listen "$socket_path" $argv
end

# tool configurations
if type -q bat
    abbr cat bat
    set -x BAT_THEME ansi
    set -x BAT_PAGER
    set -x COLORTERM truecolor
end

if status is-interactive; and type -q zoxide
    zoxide init fish | source
end

# node.js ecosystem
set -x NPM_PACKAGES "$HOME/.npm-packages"
set -l npm_global_modules "$NPM_PACKAGES/lib/node_modules"
if set -q NODE_PATH[1]; and string length -q -- "$NODE_PATH"
    set -x NODE_PATH "$npm_global_modules:$NODE_PATH"
else
    set -x NODE_PATH "$npm_global_modules"
end
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
abbr piex 'iex -S mix phx.server'

# postgresql
set -x PGDATABASE postgres

# other languages and tools
abbr lisp 'rlwrap sbcl'
fish_add_path -aP "$HOME/.local/share/lua-language-server/bin"

# cloud and devops
fish_add_path -aP "$HOME/.aws/bin"
set -x AWS_IAM_HOME "$HOME/.aws/iam"
set -x AWS_CREDENTIALS_FILE "$HOME/.aws/credentials"
fish_add_path -aP "$HOME/.docker/cli-plugins"

# development tools
fish_add_path -aP "$HOME/.opencode/bin"
abbr oc opencode

# editor integrations
if [ "$INSIDE_EMACS" = vterm ]
    . $HOME/.config/fish/vterm.fish
end

# environment managers (with performance guards)
# Use mise shims only: avoids per-shell activation/env hooks while preserving
# access to mise-managed tools. Regenerate completions with:
# mise completion fish > ~/.config/fish/completions/mise.fish
if test -d "$HOME/.local/share/mise/shims"
    fish_add_path -pP "$HOME/.local/share/mise/shims"
end


