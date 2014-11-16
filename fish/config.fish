# functions who work as aliases (quicker)
function t1; tree --dirsfirst -ChFL 1; end
function t2; tree --dirsfirst -ChFL 2; end
function t3; tree --dirsfirst -ChFL 3; end
function wl; wicd-curses; end
function weechat; weechat-curses $argv; end
function nstat; sudo nethogs wlan0 $argv; end
function duh; du -ah --max-depth=1; end
function lah; ls -lah; end
function j; cd (command autojump $argv); end
function e; emacsclient -a "vim" -t $argv; end
function v; vim $argv; end
function gh-preview; python -m grip; end
function gogo; mosh ubuntu.local; end

# start end end dropbox
function dropstart; sudo systemctl start dropbox@wunki.service; end
function dropstop; sudo systemctl stop dropbox@wunki.service; end

# git
function gs; git status --ignore-submodules=dirty; end
function gp; git push origin master; end
function gf; git pull origin master; end

# rust
function rust-update; curl https://static.rust-lang.org/rustup.sh | bash; end

# erlang
function erlr; erl -pz ebin deps/*/ebin $argv; end

# python
function rmpyc; find . -name '*.pyc' | xargs rm; end

# run different databases
function redis-run; redis-server /usr/local/etc/redis.conf; end
function influx-run; influxdb -config=/usr/local/etc/influxdb.conf; end
function zookeeper-run; zkServer start; end
function mongo-run; mongod --config /usr/local/etc/mongod.conf; end
function kafka-run; kafka-server-start.sh /usr/local/etc/kafka/server.properties; end

# NFS
function nfsstart; sudo systemctl start rpc-idmapd rpc-mountd; end

# Mu indexing
function mu-reindex; mu index --rebuild --maildir=/Users/wunki/Mail --my-address=petar@wunki.org --my-address=petar@gibbon.co --my-address=petar@breadandpepper.com --my-address=hello@gibbon.co --my-address=hello@breadandpepper.com; end
function mu-index; mu index --maildir=/Users/wunki/Mail --my-address=petar@wunki.org --my-address=petar@gibbon.co --my-address=petar@breadandpepper.com --my-address=hello@gibbon.co --my-address=hello@breadandpepper.com; end

# environment variables
set -x fish_greeting ""
set -x EDITOR 'emacsclient -t -a ""'
set -x VISUAL 'emacsclient -t -a ""'
set -x TERM 'screen-256color'

# secret environment vars
. ~/.config/fish/secret_env.fish

# autojump
. ~/.config/fish/autojump.fish

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

# clojure
set -x LEIN_JAVA_CMD "$HOME/.bin/drip"

# go
if test -d "$HOME/go"
   set -x GOPATH "$HOME/go"
else
   set -x GOPATH "$HOME/Go"
end
prepend_to_path "$GOPATH/bin"

if test -x "$GOPATH/bin/hub"
   function git; hub $argv; end
end

# boot2docker on the mac
if contains (hostname -s) "macbook"
    set -x DOCKER_HOST "tcp://192.168.59.103:2376"
    set -x DOCKER_CERT_PATH "/Users/wunki/.boot2docker/certs/boot2docker-vm"
    set -x DOCKER_TLS_VERIFY 1
end


# rust
set -x RUST_SRC_PATH "/Users/wunki/Rust/rust/src"

# rubygems
prepend_to_path "$HOME/.gem/ruby/2.0.0/bin"
prepend_to_path "$HOME/.gem/ruby/1.9.1/bin"

# android
prepend_to_path "/opt/android-sdk/tools"
prepend_to_path "/opt/android-sdk/platform-tools"

# perl
prepend_to_path "/usr/bin/core_perl"

# python
prepend_to_path "$HOME/Library/Python/2.7/bin"
prepend_to_path "$HOME/.pyenv/bin"
status --is-interactive; and . (pyenv init -|psub)
status --is-interactive; and . (pyenv virtualenv-init -|psub)
set -gx PYTHONPATH "$HOME/Library/Python/2.7/lib/python/site-packages:/Library/Python/2.7/site-packages"

# git prompt
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch yellow

# aws
prepend_to_path "$HOME/.aws/bin"
set -x AWS_IAM_HOME "$HOME/.aws/iam"
set -x AWS_CREDENTIALS_FILE "$HOME/.aws/credentials"

# status chars
set __fish_git_prompt_char_upstream_equal '✓'
set __fish_git_prompt_char_dirtystate '⚡'
set __fish_git_prompt_char_stagedstate '→'
set __fish_git_prompt_char_stashstate '↩'
set __fish_git_prompt_char_upstream_ahead '↑'
set __fish_git_prompt_char_upstream_behind '↓'

# set variables on directories with ondir
if test -f /usr/local/bin/ondir
    function ondir_prompt_hook --on-event fish_prompt
        if test ! -e "$OLDONDIRWD"; set -g OLDONDIRWD /; end;
        if [ "$OLDONDIRWD" != "$PWD" ]; eval (ondir $OLDONDIRWD $PWD); end;
        set -g OLDONDIRWD "$PWD";
    end
end

# the prompt
function fish_prompt
  set last_status $status

  # CWD
  set_color $fish_color_cwd
  printf '%s' (prompt_pwd)

  # Git
  set_color normal
  printf '%s ' (__fish_git_prompt)
  set_color normal
end
