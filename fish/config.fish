# Keep startup quiet.
set -g fish_greeting

# environment
set -x LANG 'en_US.UTF-8'
set -x XDG_DATA_HOME "$HOME/.local/share"
if status is-interactive
    set -l gpg_tty (tty 2>/dev/null)
    if test $status -eq 0; and test -n "$gpg_tty"
        set -x GPG_TTY $gpg_tty
    end
end

# Keep secrets outside the repository.
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

# Zed terminal
if test "$TERM_PROGRAM" = zed
    set -x EDITOR zed
    set -x VISUAL zed --wait
end

# local paths
fish_add_path -aP "$HOME/.local/bin"

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
if type -q eza; and test -r ~/.config/eza/cendre.fish
    source ~/.config/eza/cendre.fish
end

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

# Small shell helpers
function mkcd
    if test (count $argv) -eq 0
        echo "mkcd: missing directory"
        return 1
    end

    mkdir -p -- $argv[1]; and cd -- $argv[1]
end

# Give each tmux session a Neovim socket for nvim:// links.
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

# Advertise 24-bit color to tools that check for it.
set -x COLORTERM truecolor

if type -q bat
    abbr cat bat
end

if status is-interactive; and type -q zoxide
    # Standalone Fish embeds cd.fish, while zoxide 0.9.8 expects it on disk.
    if not functions --query __zoxide_cd_internal
        functions --copy cd __zoxide_cd_internal
    end
    zoxide init fish | source
end

# pnpm
set -x PNPM_HOME "$HOME/.local/share/pnpm"
fish_add_path -aP "$PNPM_HOME"
abbr pp pnpm
abbr ppx pnpx

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

# Partition os_deps compile work by CPU cores / 2. The core count is cached in
# a universal variable because sysctl costs a few milliseconds per startup on
# macOS; erase __cpu_count after moving this config to different hardware.
if not string match -qr '^[0-9]+$' -- "$__cpu_count"
    set -l detected
    if type -q nproc
        set detected (nproc 2>/dev/null)
    else if type -q sysctl
        set detected (sysctl -n hw.physicalcpu 2>/dev/null)
    end
    if string match -qr '^[0-9]+$' -- "$detected"
        set -U __cpu_count $detected
    end
end
if string match -qr '^[0-9]+$' -- "$__cpu_count"
    set -x MIX_OS_DEPS_COMPILE_PARTITION_COUNT (math --scale=0 "max(1, $__cpu_count / 2)")
end

# postgresql
set -x PGDATABASE postgres

# other languages and tools
function lisp --description 'Start an SBCL terminal REPL with Linedit'
    command sbcl $argv \
        --eval '(ql:quickload "linedit" :silent t)' \
        --eval '(linedit:install-repl :wrap-current t :eof-quits t)'
end

# docker
fish_add_path -aP "$HOME/.docker/cli-plugins"

# development tools
fish_add_path -aP "$HOME/.opencode/bin"
abbr oc opencode

# Use mise shims without running activation hooks in every shell.
# Regenerate completions with:
# mise completion fish > ~/.config/fish/completions/mise.fish
if test -d "$HOME/.local/share/mise/shims"
    fish_add_path -pP "$HOME/.local/share/mise/shims"
end
