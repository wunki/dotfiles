set -U brew_prefix (brew --prefix)

# Setup homebrew
if status is-interactive
  eval (/opt/homebrew/bin/brew shellenv)
end

# Mac specific paths
fish_add_path -aP /opt/homebrew/bin
fish_add_path -aP $brew_prefix/opt/node@14/bin
fish_add_path -aP $brew_prefix/opt/python@3.9/libexec/bin
fish_add_path -aP /Applications/Postgres.app/Contents/Versions/latest/bin

# Emacs
fish_add_path -aP /Applications/Emacs.app/Contents/MacOS
fish_add_path -aP /Applications/Emacs.app/Contents/MacOS/bin

set -x OPENSSL_INCLUDE_DIR $brew_prefix/opt/openssl/include
set -x OPENSSL_LIB $brew_prefix/opt/openssl/lib
set -x OPENSSL_ROOT_DIR $brew_prefix/opt/openssl

function bup --description "Updates, upgrades and cleanes Homebrew"
    brew update
    brew upgrade
    brew cleanup
end
