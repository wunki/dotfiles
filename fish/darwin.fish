# Setup homebrew
# Stopped running this because it considerably slowed down my Fish shell.
#if status is-interactive
  # eval (/opt/homebrew/bin/brew shellenv)
#end

set -U brew_prefix /opt/homebrew

# Mac specific paths
fish_add_path -aP $brew_prefix/bin
fish_add_path -aP $brew_prefix/opt/node@14/bin
fish_add_path -aP $brew_prefix/opt/python@3.9/libexec/bin
fish_add_path -aP /Applications/Postgres.app/Contents/Versions/latest/bin
fish_add_path -aP /Applications/Racket/bin

# Emacs
fish_add_path -aP /Applications/Emacs.app/Contents/MacOS
fish_add_path -aP /Applications/Emacs.app/Contents/MacOS/bin

# Ruby
fish_add_path -aP $brew_prefix/opt/ruby/bin
fish_add_path -aP $brew_prefix/lib/ruby/gems/3.2.0/bin

# PostgreSQL
fish_add_path -aP $brew_prefix/opt/libpq/bin

set -x OPENSSL_INCLUDE_DIR $brew_prefix/opt/openssl/include
set -x OPENSSL_LIB $brew_prefix/opt/openssl/lib
set -x OPENSSL_ROOT_DIR $brew_prefix/opt/openssl

function bup --description "Updates, upgrades and cleans Homebrew"
    brew update
    brew upgrade
    brew cleanup
end
