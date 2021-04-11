# General settings
set -x fish_greeting ""

# Don't show any message on start
function fish_title
  true
end

# Notify me when a command is not found
function __fish_default_command_not_found_handler --on-event fish_command_not_found
  functions --erase __fish_command_not_found_setup
  echo "'$argv' not found"
end

# Shortcuts
function bup; brew update; and brew upgrade; and brew cleanup; end
function t1; tree --dirsfirst -ChFL 1; end
function t2; tree --dirsfirst -ChFL 2; end
function t3; tree --dirsfirst -ChFL 3; end
function gs; git status --ignore-submodules=dirty; end
function gp; git push origin master; end
function gf; git pull origin master; end
function e; emacsclient --no-wait --create-frame --quiet -a emacs $argv; end
function v; nvim $argv; end
function sv; doas nvim $argv; end
function clip; xclip -selection clipboard $argv; end
function open-ports; sudo lsof -PiTCP -sTCP:LISTEN; end

if contains (uname -s) "Linux"
  abbr -a -g paco 'sudo pacman -Rs (pacman -Qqtd)'
end

# Use EXA for listing files
if type -q exa
  abbr -a -g l 'exa'
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

# Elixir
abbr -a -g miex 'iex -S mix'
abbr -a -g mtm 'mix test --only module:'
set -x ERL_AFLAGS "-kernel shell_history enabled"

# Override term for SSH
function ssh; env TERM=xterm-256color ssh $argv; end

# PostgreSQL -- don't go to the users database which never exists...
set -x PGDATABASE "postgres"

# Use nvim when installed
if type -q nvim
  function vim; nvim $argv; end
end

# Environment variables
if contains (uname -s) "Darwin"
  set -x PROJECT_DIR {$HOME}/Code
else if contains (uname -s) "FreeBSD"
  set -x PROJECT_DIR {$HOME}/src
else
  set -x PROJECT_DIR {$HOME}/code
end
set -x LANG 'en_US.UTF-8'
set -x LC_ALL 'en_US.UTF-8'
set -x EDITOR 'nvim'
set -x VISUAL 'nvim'
set -x XDG_DATA_HOME {$HOME}/.local/share
set -x GPG_TTY (tty)

function prepend_to_path -d "Prepend the given dir to PATH if it exists and is not already in it"
  if test -d $argv[1]
    if not contains $argv[1] $PATH
      set -gx PATH "$argv[1]" $PATH
    end
  end
end

function curl_time -d "Measure the response time for a given URL"
    curl -so /dev/null -w "\
   namelookup:  %{time_namelookup}s\n\
      connect:  %{time_connect}s\n\
   appconnect:  %{time_appconnect}s\n\
  pretransfer:  %{time_pretransfer}s\n\
     redirect:  %{time_redirect}s\n\
starttransfer:  %{time_starttransfer}s\n\
-------------------------\n\
total:  %{time_total}s\n" $argv
end

set -e PATH # Cleans out the path variable
prepend_to_path "/bin"
prepend_to_path "/sbin"
prepend_to_path "/usr/bin"
prepend_to_path "/usr/sbin"
prepend_to_path "/usr/local/sbin"
prepend_to_path "/usr/local/bin"
prepend_to_path "/usr/local/opt/go/libexec/bin"
prepend_to_path "/usr/local/opt/mono/bin"
prepend_to_path "/usr/bin/core_perl"
prepend_to_path "$HOME/.bin"
prepend_to_path "$HOME/bin"
prepend_to_path "$HOME/.local/bin"
prepend_to_path "$HOME/.cabal/bin"
prepend_to_path "$HOME/.npm/bin"
prepend_to_path "$HOME/.npm-packages/bin"
prepend_to_path "$HOME/.rbenv/shims"
prepend_to_path "$HOME/.yarn/bin"
prepend_to_path "/usr/local/opt/gettext/bin"
prepend_to_path "$HOME/Library/Python/3.7/bin"
prepend_to_path "$HOME/.local/share/zig"
prepend_to_path "/usr/local/share/dotnet"
prepend_to_path "/Applications/Emacs.app/Contents/MacOS/bin"
prepend_to_path "/mnt/c/Program Files/Docker/Docker/resources/bin"
prepend_to_path "/mnt/c/Users/Petar Radosevic/AppData/Local/Programs/Microsoft VS Code/bin"

# Emacs on the Mac
if test -d "/Applications/Emacs.app"
    function emacs; /Applications/Emacs.app/Contents/MacOS/Emacs -nw; end
end

# Google Cloud SDK
prepend_to_path "$HOME/.local/google-cloud-sdk/bin"
if test -d "$HOME/.local/google-cloud-sdk/"
    . "$HOME/.local/google-cloud-sdk/path.fish.inc"
end

# Ripgrep and FZF
set -x FZF_DEFAULT_COMMAND 'rg --files --hidden --follow -g "!.git/" 2> /dev/null'
set -x FZF_CTRL_T_COMMAND "$FZF_DEFAULT_COMMAND"

if contains (uname -s) "Darwin"
  set -x FZF_ALT_C_COMMAND 'cd ~/; bfs -type d -nohidden'
end

if test -d "$HOME/.fzf"
  prepend_to_path "$HOME/.fzf/bin"
  . "$HOME/.fzf/shell/key-bindings.fish"
end

# Mac specific settings
if contains (uname -s) "Darwin"
  . "$HOME/.config/fish/functions/mac.fish"

  prepend_to_path "/Applications/Postgres.app/Contents/Versions/11.0/bin"
end

# Rust
prepend_to_path "$HOME/.cargo/bin"

# Erlang
export KERL_CONFIGURE_OPTIONS="--disable-debug --without-javac"
prepend_to_path "/usr/local/lib/erlang23/bin"

# Set the correct path with rustup
if type -q rustc
  set -x RUST_SRC_PATH (rustc --print sysroot)"/lib/rustlib/src/rust/src"
end

# Go
set -x GOPATH "$PROJECT_DIR/go"
prepend_to_path "/usr/local/go/bin"
prepend_to_path "$GOPATH/bin"
function gb; go build; end
function gt; go test -v ./...; end
function gc; gocov test | gocov report; end

# NodeJS
set -x NPM_PACKAGES "$HOME/.npm-packages"
set -x NODE_PATH "$NPM_PACKAGES/lib/node_modules:$NODE_PATH"
prepend_to_path "$NPM_PACKAGES/bin"

# Ruby
prepend_to_path "$HOME/.gem/ruby/2.5.0/bin"
prepend_to_path "$HOME/.gem/ruby/2.4.0/bin"

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

# Hub: easy access to github
if type -q hub
  function git; hub $argv; end
end

if test -d "$HOME/.asdf"
  source $HOME/.asdf/asdf.fish
end

# Configuration specific to WSL2
if string match -q "*microsoft*" (uname -a)
  set -x DISPLAY (cat /etc/resolv.conf | grep nameserver | awk '{print $2; exit;}'):0.0
  set -x GDK_SCALE 0.5
  set -x GDK_DPI_SCALE 2
  set -x PYTHON_KEYRING_BACKEND keyring.backends.null.Keyring
  keychain --eval --quiet --agents ssh id_rsa | source
end

