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
function mux; tmuxinator $argv; end
function ghp; python -m grip; end
function dino; mosh dino.wunki.org -- fish; end
function e; emacsclient -nq -a emacs $argv; end
function et; emacsclient -t; end
function v; nvim $argv; end
function clip; xclip -selection clipboard $argv; end

if contains (uname -s) "Linux"
  abbr -a -g paco 'sudo pacman -Rs (pacman -Qqtd)'
end

if type -Pq exa
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

if type -Pq bat
  abbr -a -g cat 'bat'
end

# Override term for SSH
function ssh; env TERM=xterm-256color ssh $argv; end

# OpenVPN
function start-vpn; sudo systemctl start openvpn-client@$argv.service; end
function stop-vpn; sudo systemctl stop openvpn-client@$argv.service; end

set -x NOMAD_ADDR "http://172.16.1.1:4646"
set -x CONSUL_HTTP_ADDR "172.16.1.1:8500"

# Use nvim when installed
if type -Pq nvim
  function vim; nvim $argv; end
end

# Environment variables
if contains (uname -s) "Darwin"
  set -x PROJECT_DIR {$HOME}/Code
else
  set -x PROJECT_DIR {$HOME}/projects
end
set -x LANG 'en_US.UTF-8'
set -x LC_ALL 'en_US.UTF-8'
set -x EDITOR 'nvim'
set -x BROWSER 'firefox'
set -x VISUAL 'nvim'
set -x XDG_DATA_HOME {$HOME}/.local/share
set -x GPG_TTY (tty)

# Mu: mail search
alias mu-reindex 'mu index --rebuild --maildir=~/mail --my-address=petar@wunki.org --my-address=petar@degreed.com --my-address=petar@gibbon.co --my-address=petar@breadandpepper.com --my-address=hello@gibbon.co --my-address=hello@breadandpepper.com'
alias mu-index 'mu index --maildir=~/mail --my-address=petar@wunki.org --my-address=petar@degreed.com --my-address=petar@gibbon.co --my-address=petar@breadandpepper.com --my-address=hello@gibbon.co --my-address=hello@breadandpepper.com'

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
prepend_to_path "$HOME/Library/Python/3.7/bin/"
prepend_to_path "/usr/local/share/dotnet"

# Google Cloud SDK
prepend_to_path "$HOME/.google-cloud-sdk/bin"
if test -d "$HOME/Code/google-cloud-sdk/"
    . "$HOME/Code/google-cloud-sdk/path.fish.inc"
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

# Unix specific settings
if contains (uname -s) "Linux"
  function alafull; wmctrl -r 'Alacritty' -b toggle,fullscreen; end
  bind $argv \ef alafull
end

# Autjump: Quickly jump to directories
if test -f "/usr/local/share/autojump/autojump.fish"
  . "/usr/local/share/autojump/autojump.fish"
end

if test -f "$HOME/.autojump/share/autojump/autojump.fish"
  . "$HOME/.autojump/share/autojump/autojump.fish"
end

# Rust
prepend_to_path "$HOME/.cargo/bin"
function ct; cargo test -- --nocapture; end
function ctl; cargo test --lib -- --nocapture $argv; end
function cb; cargo build; end
function cdoc; cargo doc --no-deps --open; end
function clippy; cargo +nightly clippy; end
function rust-musl-builder; docker run --rm -it -v "$PWD":/home/rust/src ekidd/rust-musl-builder; end
set -x LD_LIBRARY_PATH {LD_LIBRARY_PATH}:/usr/local/lib

# Clojure
set -x BOOT_CLOJURE_VERSION "1.9.0"

# Set the correct path with rustup
if type -Pq rustc
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
if type -Pq pyenv
  status --is-interactive; and source (rbenv init -|psub)
end

# Python
prepend_to_path "$HOME/.pyenv/bin"
if type -Pq pyenv
  status --is-interactive; and . (pyenv init -|psub)
  status --is-interactive; and . (pyenv virtualenv-init -|psub)
end

# Dotnet
set -x DOTNET_CLI_TELEMETRY_OPTOUT "true"
set -x DOTNET_SKIP_FIRST_TIME_EXPERIENCE "true"

# AWS settings
prepend_to_path "$HOME/.aws/bin"
set -x AWS_IAM_HOME "$HOME/.aws/iam"
set -x AWS_CREDENTIALS_FILE "$HOME/.aws/credentials"

# Direnv: adding environment variables per directory in a .envrc
if type -Pq direnv
  eval (direnv hook fish)
end

# Hub: easy access to github
if type -Pq hub
  function git; hub $argv; end
end

# Autojump: quickly jump to directories
if test -f "/usr/share/autojump/autojump.fish"
  . "/usr/share/autojump/autojump.fish"
end
