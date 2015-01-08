# functions who work as aliases (quicker)
function t1; tree --dirsfirst -ChFL 1; end
function t2; tree --dirsfirst -ChFL 2; end
function t3; tree --dirsfirst -ChFL 3; end
function wl; wicd-curses; end
function weechat; weechat-curses $argv; end
function nstat; sudo nethogs wlan0 $argv; end
function duh; du -ah --max-depth=1; end
function lah; ls -lah; end
function et; emacsclient -a "" -t $argv; end
function e; emacsclient -a "" -nq $argv; end
function v; vim $argv; end
function gh-preview; python -m grip; end

# mu
function mu-reindex; mu index --rebuild --maildir=~/mail --my-address=petar@wunki.org --my-address=petar@gibbon.co --my-address=petar@breadandpepper.com --my-address=hello@gibbon.co --my-address=hello@breadandpepper.com; end
function mu-index; mu index --maildir=~/mail --my-address=petar@wunki.org --my-address=petar@gibbon.co --my-address=petar@breadandpepper.com --my-address=hello@gibbon.co --my-address=hello@breadandpepper.com; end

# git
function gs; git status --ignore-submodules=dirty; end
function gp; git push origin master; end
function gf; git pull origin master; end

# rust
set -x LD_LIBRARY_PATH {$LD_LIBRARY_PATH}:/usr/local/lib
function rust-update; curl https://static.rust-lang.org/rustup.sh | sudo bash; end

# erlang
function erlr; erl -pz ebin deps/*/ebin $argv; end

# python
function rmpyc; find . -name '*.pyc' | xargs rm; end

# environment variables
set -x fish_greeting ""
set -x EDITOR 'emacsclient -t -a ""'
set -x VISUAL 'emacsclient -t -a ""'
set -x TERM 'rxvt-256color'
set -x XDG_DATA_HOME {$HOME}/.local/share

# UTF-8
set -x LANG 'en_US.UTF-8'
set -x LC_ALL 'en_US.UTF-8'

# secret environment vars
set fish_secret "~/.config/fish/secret_env.fish"
if test -f $fish_secret
  . $fish_secret
end  

# autojump
if contains (hostname -s) "macbook"
  set autojump_path "/usr/local/etc/autojump.fish"
else
  set autojump_path "/etc/profile.d/autojump.fish"
end
if test -f $autojump_path
  . $autojump_path
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
prepend_to_path "$HOME/bin"
prepend_to_path "$HOME/.bin"
prepend_to_path "$HOME/.local/bin"
prepend_to_path "/usr/local/opt/go/libexec/bin"

# mac specific paths
prepend_to_path "/usr/local/Cellar/emacs/HEAD/bin"
prepend_to_path "$HOME/Source/google-cloud-sdk/bin"
prepend_to_path "/Applications/Postgres.app/Contents/Versions/9.3/bin"

# haskell
if contains (hostname -s) "macbook"
  set -x GHC_DOT_APP "/Applications/ghc-7.8.3.app"
  prepend_to_path "$GHC_DOT_APP/Contents/bin"
end
prepend_to_path "$HOME/.cabal/bin"

# go
prepend_to_path "/usr/local/go/bin"
if contains (hostname -s) "macbook"
  set -x GOMAXPROCS (sysctl hw.ncpu | awk '{print $2}')
else
  set -x GOMAXPROCS (nproc)
end

if test -d "$HOME/go"
  set -x GOPATH "$HOME/go"
else
  set -x GOPATH "$HOME/Go"
end
prepend_to_path "$GOPATH/bin"

# nodejs
if test -f ~/.nvm-fish/nvm.fish
  source ~/.nvm-fish/nvm.fish
end

if test -x "$GOPATH/bin/hub"
  function git; hub $argv; end
end

# boot2docker on the mac
if contains (hostname -s) "macbook"
  set -x DOCKER_HOST "tcp://192.168.59.103:2376"
  set -x DOCKER_CERT_PATH "/Users/wunki/.boot2docker/certs/boot2docker-vm"
  set -x DOCKER_TLS_VERIFY 1
end

# racket (mac)
prepend_to_path "/Applications/Racket v6.1.1/bin"

# rubygems
prepend_to_path "$HOME/.gem/ruby/2.0.0/bin"
prepend_to_path "$HOME/.gem/ruby/1.9.1/bin"

# android
prepend_to_path "/opt/android-sdk/tools"
prepend_to_path "/opt/android-sdk/platform-tools"

# perl
prepend_to_path "/usr/bin/core_perl"

# python
prepend_to_path "$HOME/.pyenv/bin"
# status --is-interactive; and . (pyenv init -|psub)
# status --is-interactive; and . (pyenv virtualenv-init -|psub)

if contains (hostname -s) "macbook"
  prepend_to_path "$HOME/Library/Python/2.7/bin"  
  set -gx PYTHONPATH "$HOME/Library/Python/2.7/lib/python/site-packages:/Library/Python/2.7/site-packages"
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
if test -f /usr/sbin/ondir; or test -f /usr/local/bin/ondir
  function ondir_prompt_hook --on-event fish_prompt
  if test ! -e "$OLDONDIRWD"; set -g OLDONDIRWD /; end;
  if [ "$OLDONDIRWD" != "$PWD" ]; eval (ondir $OLDONDIRWD $PWD); end;
    set -g OLDONDIRWD "$PWD";
  end
end
