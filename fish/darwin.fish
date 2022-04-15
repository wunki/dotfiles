# Setup homebrew
if status is-interactive
  eval (/opt/homebrew/bin/brew shellenv)
end

# Mac specific paths
fish_add_path -aP /opt/homebrew/bin
fish_add_path -aP (brew --prefix)/opt/node@14/bin
fish_add_path -aP (brew --prefix)/opt/python@3.9/libexec/bin

set -x TERMINFO "/Applications/kitty.app/Contents/Resources/kitty/terminfo"
set -x SHELL xterm-kitty
set -x PROJECT_DIR {$HOME}/Code
set -x OPENSSL_INCLUDE_DIR (brew --prefix)/opt/openssl/include
set -x OPENSSL_LIB (brew --prefix)/opt/openssl/lib
set -x OPENSSL_ROOT_DIR (brew --prefix)/opt/openssl

function bup --description "Updates, upgrades and cleanes Homebrew"
    brew update
    brew upgrade
    brew cleanup
end

# ASDF
test -d (brew --prefix)"/opt/asdf" ; and source (brew --prefix)/opt/asdf/libexec/asdf.fish
