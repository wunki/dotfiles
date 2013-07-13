# functions who work as aliases (quicker)
function t1; tree --dirsfirst -ChFL 1; end
function t2; tree --dirsfirst -ChFL 2; end
function t3; tree --dirsfirst -ChFL 3; end
function gsdw; sudo ~/bin/get-shit-done work; end
function gsdp; sudo ~/bin/get-shit-done play; end
function wl; wicd-curses; end
function weechat; weechat-curses $argv; end
function nstat; sudo nethogs wlan0 $argv; end
function duh; du -ah --max-depth=1; end

# start end end dropbox
function dropstart; sudo systemctl start dropbox@wunki.service; end
function dropstop; sudo systemctl stop dropbox@wunki.service; end

# git
function gs; git status --ignore-submodules=dirty; end
function gp; git push origin master; end
function gf; git pull origin master; end

# erlang
function erlr; erl -pz ebin deps/*/ebin $argv; end

# python
function rmpyc; find . -name '*.pyc' | xargs rm; end

# environment variables
set -x fish_greeting ""
set -x EDITOR 'emacsclient -t -a emacs'
set -x VISUAL 'emacsclient -t -a emacs'
set -x BROWSER /home/wunki/bin/conk

function prepend_to_path -d "Prepend the given dir to PATH if it exists and is not already in it"
    if test -d $argv[1]
        if not contains $argv[1] $PATH
            set -gx PATH "$argv[1]" $PATH
        end
    end
end
prepend_to_path "/bin"
prepend_to_path "/sbin"
prepend_to_path "/usr/bin"
prepend_to_path "/usr/sbin"
prepend_to_path "/usr/local/bin"
prepend_to_path "/usr/local/sbin"
prepend_to_path "$HOME/bin"
prepend_to_path "$HOME/.local/bin"

# haskell
prepend_to_path "$HOME/.cabal/.bin"

# python
set -x PIP_DOWNLOAD_CACHE "$HOME/.pip/cache"
set -x WORKON_HOME "$HOME/.virtualenvs"
. ~/src/virtualfish/virtual.fish

# git prompt
# set __fish_git_prompt_showdirtystate 'yes'
# set __fish_git_prompt_showstashstate 'yes'
# set __fish_git_prompt_showupstream 'yes'
# set __fish_git_prompt_color_branch yellow

# status chars
# set __fish_git_prompt_char_upstream_equal '✓'
# set __fish_git_prompt_char_dirtystate '⚡'
# set __fish_git_prompt_char_stagedstate '→'
# set __fish_git_prompt_char_stashstate '↩'
# set __fish_git_prompt_char_upstream_ahead '↑'
# set __fish_git_prompt_char_upstream_behind '↓'

# the prompt
function fish_prompt
  set last_status $status

  # python virtualenv
  if set -q VIRTUAL_ENV
    echo -n -s "(" (basename "$VIRTUAL_ENV") ") "
  end

  set_color $fish_color_cwd
  printf '%s ' (prompt_pwd)
  # set_color normal
  # printf '%s ' (__fish_git_prompt)
  set_color normal
end
