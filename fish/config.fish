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

# Tunnels
function kafka-tunnel; ssh -L 9092:172.16.1.18:9092 -N dino.wunki.org; end
function syncthing-tunnel; ssh -L 8385:127.0.0.1:8384 -N dino.wunki.org; end

# OpenVPN
function start-vpn; sudo systemctl start openvpn-client@$argv.service; end
function stop-vpn; sudo systemctl stop openvpn-client@$argv.service; end

# Use nvim when installed
if type -Pq nvim
  function vim; nvim $argv; end
end

# Environment variables
set -x PROJECT_DIR {$HOME}/projects
set -x LANG 'en_US.UTF-8'
set -x LC_ALL 'en_US.UTF-8'
set -x EDITOR 'nvim'
set -x VISUAL 'nvim'
set -x TERM 'xterm-256color'
set -x XDG_DATA_HOME {$HOME}/.local/share


# Ripgrep and FZF
set -x FZF_DEFAULT_COMMAND 'rg --files --hidden --smart-case --glob "!.git/*"'

if test -d "/usr/local/opt/fzf"
  . "/usr/local/opt/fzf/shell/key-bindings.fish"
end

# Mu: mail search
function mu-reindex; mu index --rebuild --maildir=~/mail --my-address=petar@wunki.org --my-address=petar@gibbon.co --my-address=petar@breadandpepper.com --my-address=hello@gibbon.co --my-address=hello@breadandpepper.com; end
function mu-index; mu index --maildir=~/mail --my-address=petar@wunki.org --my-address=petar@gibbon.co --my-address=petar@breadandpepper.com --my-address=hello@gibbon.co --my-address=hello@breadandpepper.com; end

function prepend_to_path -d "Prepend the given dir to PATH if it exists and is not already in it"
  if test -d $argv[1]
    if not contains $argv[1] $PATH
      set -gx PATH "$argv[1]" $PATH
    end
  end
end

set -e PATH # Cleans out the path variable
prepend_to_path "/bin"
prepend_to_path "/sbin"
prepend_to_path "/usr/bin"
prepend_to_path "/usr/sbin"
prepend_to_path "/usr/local/sbin"
prepend_to_path "/usr/local/bin"
prepend_to_path "/usr/local/opt/go/libexec/bin"
prepend_to_path "/usr/bin/core_perl"
prepend_to_path "$HOME/.bin"
prepend_to_path "$HOME/bin"
prepend_to_path "$HOME/.local/bin"
prepend_to_path "$HOME/.cabal/bin"
prepend_to_path "$HOME/.npm/bin"

# Google Cloud SDK
prepend_to_path "$HOME/.google-cloud-sdk/bin"

# Mac specific settings
if contains (uname -s) "Darwin"
  . "$HOME/.config/fish/functions/mac.fish"
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
function clippy; rustup run nightly cargo clippy; end
function rust-musl-builder; docker run --rm -it -v "$PWD":/home/rust/src ekidd/rust-musl-builder; end
set -x LD_LIBRARY_PATH {LD_LIBRARY_PATH}:/usr/local/lib

# Erlang and Elixir
set -x ERL_AFLAGS "-kernel shell_history enabled"

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

# Clojure
set -x BOOT_COLOR 1
set -x BOOT_JVM_OPTIONS "-Xmx2g -client -XX:+TieredCompilation -XX:TieredStopAtLevel=1 -XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled -Xverify:none"
set -x BOOT_CLOJURE_VERSION "1.9.0-alpha14"

# NodeJS
set -x NPM_PACKAGES "$HOME/.npm-packages"
set -x NODE_PATH "$NPM_PACKAGES/lib/node_modules:$NODE_PATH"
prepend_to_path "$NPM_PACKAGES/bin"

# Ruby
prepend_to_path "$HOME/.gem/ruby/2.4.0/bin"
prepend_to_path "$HOME/.gem/ruby/2.3/bin"

# Python
prepend_to_path "$HOME/.pyenv/bin"
if type -Pq pyenv
  status --is-interactive; and . (pyenv init -|psub)
  status --is-interactive; and . (pyenv virtualenv-init -|psub)
end

# AWS settings
prepend_to_path "$HOME/.aws/bin"
set -x AWS_IAM_HOME "$HOME/.aws/iam"
set -x AWS_CREDENTIALS_FILE "$HOME/.aws/credentials"

# Fuck: correcting mistyped stuff
if type -Pq thefuck
  eval (thefuck --alias | tr '\n' ';')
end

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
