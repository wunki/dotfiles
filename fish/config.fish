# functiones
function rmpyc; find . -name '*.pyc' | xargs rm; end
function gs; git status --ignore-submodules=dirty; end
function gp; git push origin master; end
function gf; git pull origin master; end
function l1; tree --dirsfirst -ChFL 1; end
function l2; tree --dirsfirst -ChFL 2; end
function l3; tree --dirsfirst -ChFL 3; end
function gsdw; sudo ~/bin/get-shit-done work; end
function gsdp; sudo ~/bin/get-shit-done play; end
function wl; wicd-curses; end
function weechat; weechat-curses $argv; end
function res; erl -pz ebin deps/*/ebin $argv; end

# Environment variables
set -g -x fish_greeting ""

set -g -x EDITOR vim

function prepend_to_path -d "Prepend the given dir to PATH if it exists and is not already in it"
    if test -d $argv[1]
        if not contains $argv[1] $PATH
            set -gx PATH "$argv[1]" $PATH
        end
    end
end
prepend_to_path "/sbin"
prepend_to_path "/usr/sbin"
prepend_to_path "/bin"
prepend_to_path "/usr/bin"
prepend_to_path "/usr/local/bin"
prepend_to_path "/usr/local/sbin"

# Python
set -g -x PIP_DOWNLOAD_CACHE "$HOME/.pip/cache"
set -g -x WORKON_HOME "$HOME/.virtualenvs"
. ~/.config/fish/virtualenv.fish

# Haskell
prepend_to_path "$HOME/bin"
prepend_to_path "$HOME/.cabal/.bin"
