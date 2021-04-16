# General settings
set -x fish_greeting ""

# Don't show any message on start
function fish_title
  true
end

# Environment
set -x LANG 'en_US.UTF-8'
set -x LC_ALL 'en_US.UTF-8'
if type -q nvim
  set -x EDITOR 'nvim'
  set -x VISUAL 'nvim'
else 
  set -x EDITOR 'vim'
  set -x VISUAL 'vim'
end
set -x XDG_DATA_HOME {$HOME}/.local/share
set -x GPG_TTY (tty)

set -e PATH # Cleans out the path variable
prepend_to_path "/bin"
prepend_to_path "/sbin"
prepend_to_path "/usr/bin"
prepend_to_path "/usr/sbin"
prepend_to_path "/usr/local/sbin"
prepend_to_path "/usr/local/bin"
prepend_to_path "$HOME/.bin"
prepend_to_path "$HOME/.local/bin"
prepend_to_path "$HOME/.npm/bin"
prepend_to_path "$HOME/.npm-packages/bin"
prepend_to_path "$HOME/.yarn/bin"
prepend_to_path "$PROJECT_DIR/zig"
prepend_to_path "/usr/local/share/dotnet"
prepend_to_path "$HOME/.cargo/bin"

# System specific configuration
switch (uname)
  case Linux
    source $HOME/.config/fish/linux.fish
  case Darwin
    source $HOME/.config/fish/darwin.fish
  case FreeBSD
    source $HOME/.config/fish/freebsd.fish
end

abbr t1 'tree --dirsfirst -ChFL 1'
abbr t2 'tree --dirsfirst -ChFL 2'
abbr t3 'tree --dirsfirst -ChFL 3'
abbr gs 'git status --ignore-submodules=dirty'
abbr gp 'git push origin HEAD'
abbr gf 'git pull origin HEAD'
abbr e  'emacsclient --no-wait --create-frame --quiet -a emacs'
abbr v  'nvim'
abbr cdr "cd (git rev-parse --show-toplevel)"

if type -q xclip
  abbr clip 'xclip -selection clipboard'
end

# Use EXA for listing files
if type -q exa
  abbr l 'exa'
  abbr -a -g ls 'exa'
  abbr -a -g ll 'exa -l'
  abbr -a -g llg 'exa -l --git'
  abbr -a -g lll 'exa -la'
else
  abbr -a -g l 'ls'
  abbr -a -g ll 'ls -l'
  abbr -a -g lll 'ls -la'
end

# Use bat because of syntax highlighting
if type -q bat
  abbr -a -g cat 'bat'
  set -x BAT_THEME "base16"
  set -x COLORTERM "truecolor"
end

# Erlang and Elixir
abbr -a -g miex 'iex -S mix'
abbr -a -g mtm 'mix test --only module:'
prepend_to_path "/usr/local/lib/erlang23/bin"

if type -q erl
  set -x ERL_AFLAGS "-kernel shell_history enabled"
end

# PostgreSQL -- don't go to the users database which never exists...
set -x PGDATABASE "postgres"

# Use nvim when installed
if type -q nvim
  abbr vim 'nvim'
end

# FZF
if type -q fzf
  set fzf_preview_dir_cmd exa --all --color=always
end

# Go
set -x GOPATH "$PROJECT_DIR/go"
prepend_to_path "$GOPATH/bin"
abbr gb 'go build'
abbr gt 'go test -v ./...'
function gc; gocov test | gocov report; end

# NodeJS
set -x NPM_PACKAGES "$HOME/.npm-packages"
set -x NODE_PATH "$NPM_PACKAGES/lib/node_modules:$NODE_PATH"
prepend_to_path "$NPM_PACKAGES/bin"

# Dotnet
set -x DOTNET_CLI_TELEMETRY_OPTOUT "true"
set -x DOTNET_SKIP_FIRST_TIME_EXPERIENCE "true"

# AWS settings
prepend_to_path "$HOME/.aws/bin"
set -x AWS_IAM_HOME "$HOME/.aws/iam"
set -x AWS_CREDENTIALS_FILE "$HOME/.aws/credentials"

# Direnv: adding environment variables per directory in a .envrc
if type -q direnv
  eval (direnv hook fish)
end

# Version manager for different languages
if test -d "$HOME/.asdf"
  source $HOME/.asdf/asdf.fish
end
