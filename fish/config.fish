# functions who work as aliases (quicker)
function t1; tree --dirsfirst -ChFL 1; end
function t2; tree --dirsfirst -ChFL 2; end
function t3; tree --dirsfirst -ChFL 3; end
function wl; wicd-curses; end
function nstat; sudo nethogs wlan0 $argv; end
function duh; du -ah --max-depth=1; end
function lah; ls -lah; end
function gh-preview; python -m grip; end
function flush-dns; sudo discoveryutil mdnsflushcache; end
function ea; sudo ezjail-admin $argv; end
function bup; brew update; and brew upgrade --all; and brew cleanup; end

# easy editing
function e
  if test -n "$INSIDE_EMACS";
    emacsclient -a "" -nq $argv;
  else
    emacsclient -a "" -t $argv;
  end
end

# neovim when available
if test -x "/usr/local/bin/nvim";
  function vim; nvim $argv; end
end
function v; vim $argv; end

# mu
function mu-reindex; mu index --rebuild --maildir=~/mail --my-address=petar@wunki.org --my-address=petar@gibbon.co --my-address=petar@breadandpepper.com --my-address=hello@gibbon.co --my-address=hello@breadandpepper.com; end
function mu-index; mu index --maildir=~/mail --my-address=petar@wunki.org --my-address=petar@gibbon.co --my-address=petar@breadandpepper.com --my-address=hello@gibbon.co --my-address=hello@breadandpepper.com; end

# git
function gs; git status --ignore-submodules=dirty; end
function gp; git push origin master; end
function gf; git pull origin master; end

# redis on the mac
function run-redis; redis-server /usr/local/etc/redis.conf; end

# rethinkdb
function run-rethinkdb; launchctl load ~/Library/LaunchAgents/homebrew.mxcl.rethinkdb.plist; end

# rust
set -x LD_LIBRARY_PATH {LD_LIBRARY_PATH}:/usr/local/lib
set -x RUST_SRC_PATH {$HOME}/rust/rust/src
function rust-update; curl https://static.rust-lang.org/rustup.sh | sh; end

# erlang
function erlr; erl -pz ebin deps/*/ebin $argv; end

# python
function rmpyc; find . -name '*.pyc' | xargs rm; end

# environment variables
set -x fish_greeting ""
set -x EDITOR 'vim'
set -x VISUAL 'vim'
set -x TERM 'rxvt-256color'
set -x XDG_DATA_HOME {$HOME}/.local/share
set -x NVIM_TUI_ENABLE_TRUE_COLOR 1


# UTF-8
set -x LANG 'en_US.UTF-8'
set -x LC_ALL 'en_US.UTF-8'

# secret environment vars
set fish_secret "~/.config/fish/secret_env.fish"
if test -f $fish_secret
  . $fish_secret
end

function prepend_to_path -d "Prepend the given dir to PATH if it exists and is not already in it"
  if test -d $argv[1]
    if not contains $argv[1] $PATH
      set -gx PATH "$argv[1]" $PATH
    end
  end
end

# Start with a clean path, because order matters
set -e PATH

# paths
prepend_to_path "/bin"
prepend_to_path "/.local/bin"
prepend_to_path "/sbin"
prepend_to_path "/usr/bin"
prepend_to_path "/usr/sbin"
prepend_to_path "/usr/local/sbin"
prepend_to_path "/usr/local/bin"
prepend_to_path "/usr/local/opt/go/libexec/bin"

# home paths
prepend_to_path "$HOME/bin"
prepend_to_path "$HOME/Bin"

# mac specific paths
prepend_to_path "/usr/local/Cellar/emacs/HEAD/bin"
prepend_to_path "$HOME/Source/google-cloud-sdk/bin"
prepend_to_path "/Applications/Postgres.app/Contents/Versions/9.4/bin"
prepend_to_path "/Applications/Racket v6.1.1/bin"

# autojump
if test -f "$HOME/.autojump/share/autojump/autojump.fish"
  prepend_to_path "$HOME/.autojump/bin"
  . "$HOME/.autojump/share/autojump/autojump.fish"
end


# haskell
prepend_to_path "$HOME/.stack/programs/x86_64-osx/ghc-7.8.4/bin"
prepend_to_path "$HOME/.cabal/bin"

# neovim on BSD
if contains (hostname -s) "home"
  set -gx VIMRUNTIME "/usr/local/share/vim/vim74"
end

# go
function gb; go build; end
function gt; go test -v ./...; end
function gc; gocov test | gocov report; end
prepend_to_path "/usr/local/go/bin"

if test -d "$HOME/Go"
    set -x GOPATH "$HOME/Go"
else
    set -x GOPATH "$HOME/go"
end
prepend_to_path "$GOPATH/bin"

# Test coverage for Go
# Use in the current project: `cover` or `cover github.com/pkg/sftp`
function cover ()
  set -l t (mktemp /tmp/gocover.XXXXX)
  go test $COVERFLAGS -coverprofile=$t $argv;and go tool cover -func=$t;and unlink $t
end

function cover-web ()
  set -l t (mktemp /tmp/gocover.XXXXX)
  go test $COVERFLAGS -coverprofile=$t $argv;and go tool cover -html=$t;and unlink $t
end

# nodejs
if test -f ~/.nvm-fish/nvm.fish
  source ~/.nvm-fish/nvm.fish
end

set hub (type -fp hub)
if test -f $hub
  function git; hub $argv; end
end

# switch to pacaur if available
set pacaur (type -fp pacaur)
if test -f $pacaur
  function pacman; pacaur $argv; end
end

# clojure
set -x BOOT_COLOR 1
set -x BOOT_JVM_OPTIONS "-Xmx2g -client -XX:+TieredCompilation -XX:TieredStopAtLevel=1 -XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled -Xverify:none"

# nodejs
set -x NPM_PACKAGES "$HOME/.npm-packages"
set -x NODE_PATH "$NPM_PACKAGES/lib/node_modules:$NODE_PATH"
prepend_to_path "$NPM_PACKAGES/bin"

# docker for the mac
if contains (hostname -s) "macbook"
  set -x DOCKER_HOST "tcp://192.168.99.100:2376"
  set -x DOCKER_MACHINE_NAME "default"
  set -x DOCKER_TLS_VERIFY 1
  set -x DOCKER_CERT_PATH "/Users/wunki/.docker/machine/machines/default"
end

# racket (mac)
prepend_to_path "/Applications/Racket v6.1.1/bin"

# rubygems
prepend_to_path "$HOME/.gem/ruby/2.2.0/bin"

# android
prepend_to_path "/opt/android-sdk/tools"
prepend_to_path "/opt/android-sdk/platform-tools"

# perl
prepend_to_path "/usr/bin/core_perl"

# python
prepend_to_path "$HOME/.pyenv/bin"
if test -d ~/.pyenv
  status --is-interactive; and . (pyenv init -|psub)
  status --is-interactive; and . (pyenv virtualenv-init -|psub)
end

if contains (hostname -s) "macbook"
  prepend_to_path "$HOME/Library/Python/2.7/bin"
  set -gx PYTHONPATH "$HOME/Library/Python/2.7/lib/python/site-packages:/Library/Python/2.7/site-packages"
end

if contains (hostname -s) "home"
  set -gx PYTHONPATH "$HOME/.local/lib/python2.7/site-packages:/usr/local/lib/python2.7/site-packages"
end

# aws
prepend_to_path "$HOME/.aws/bin"
set -x AWS_IAM_HOME "$HOME/.aws/iam"
set -x AWS_CREDENTIALS_FILE "$HOME/.aws/credentials"

# fix fish in Emacs ansi-term
function fish_title
  true
end

# set variables on directories with ondir
if test -f /usr/local/bin/ondir
  function ondir_prompt_hook --on-event fish_prompt
  if test ! -e "$OLDONDIRWD"; set -g OLDONDIRWD /; end;
  if [ "$OLDONDIRWD" != "$PWD" ]; eval (ondir $OLDONDIRWD $PWD); end;
    set -g OLDONDIRWD "$PWD";
  end
end

