set -x PROJECT_DIR {$HOME}/Code
set -x OPENSSL_INCLUDE_DIR (brew --prefix)/opt/openssl/include
set -x OPENSSL_LIB (brew --prefix)/opt/openssl/lib
set -x OPENSSL_ROOT_DIR (brew --prefix)/opt/openssl

function bup --description "Updates, upgrades and cleanes Homebrew"
    brew update
    brew upgrade
    brew cleanup
end

# Emacs on the Mac
if test -d "/Applications/Emacs.app"
    abbr emacs '/Applications/Emacs.app/Contents/MacOS/Emacs -nw'
end

fish_add_path -aP "/Applications/Emacs.app/Contents/MacOS/bin"
fish_add_path -aP /usr/local/opt/go/libexec/bin
fish_add_path -aP /usr/local/opt/mono/bin
fish_add_path -aP "/Applications/Postgres.app/Contents/Versions/11.0/bin"
fish_add_path -aP /usr/local/Cellar/emacs/HEAD/bin
fish_add_path -aP "/Applications/Postgres.app/Contents/Versions/latest/bin"
fish_add_path -aP (brew --prefix)/opt/node@14/bin

test -d (brew --prefix)"/opt/asdf" ; and source (brew --prefix)/opt/asdf/libexec/asdf.fish

if status --is-interactive
  eval (/opt/homebrew/bin/brew shellenv)
end
