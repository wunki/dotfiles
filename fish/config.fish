# Aliases
alias e "exit"
alias rmpyc "find . -name '*.pyc' | xargs rm"
alias gs "git status --ignore-submodules=dirty"
alias gp "git push origin master"
alias gf "git pull origin master"

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
prepend_to_path "$HOME/bin"
prepend_to_path "$HOME/.cabal/.bin"
