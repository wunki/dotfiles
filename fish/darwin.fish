# Setup homebrew
set -U brew_prefix /opt/homebrew

# Mac specific paths
fish_add_path -aP $brew_prefix/bin

# PostgreSQL
fish_add_path -aP /Applications/Postgres.app/Contents/Versions/latest/bin

# Racket
fish_add_path -aP /Applications/Racket\ v8.13/bin

# Python
fish_add_path -aP $brew_prefix/opt/python@3.9/libexec/bin
fish_add_path -aP $HOME/Library/Python/3.9/bin

# Emacs
fish_add_path -aP /Applications/Emacs.app/Contents/MacOS
fish_add_path -aP /Applications/Emacs.app/Contents/MacOS/bin

# Mise version manager
if command -q mise
    ~/.local/bin/mise activate fish | source
end

# Setup Tailscale
alias tailscale "/Applications/Tailscale.app/Contents/MacOS/Tailscale"

# Guile
set -x GUILE_LOAD_PATH $brew_prefix/share/guile/site/3.0
set -x GUILE_LOAD_COMPILED_PATH $brew_prefix/lib/guile/3.0/site-ccache
set -x GUILE_SYSTEM_EXTENSIONS_PATH $brew_prefix/lib/guile/3.0/extensions

# Ruby
fish_add_path -aP $brew_prefix/opt/ruby/bin
fish_add_path -aP $brew_prefix/lib/ruby/gems/3.2.0/bin

# PostgreSQL
fish_add_path -aP $brew_prefix/opt/libpq/bin
fish_add_path -aP /Applications/Postgres.app/Contents/Versions/latest/bin

set -x OPENSSL_INCLUDE_DIR $brew_prefix/opt/openssl/include
set -x OPENSSL_LIB $brew_prefix/opt/openssl/lib
set -x OPENSSL_ROOT_DIR $brew_prefix/opt/openssl

# If I'm in the ZED shell, set the editor to Zed
if set -q ZED_TERM
    set -x EDITOR 'zed --wait'
end

# Needed for Zed build
set -gx BINDGEN_EXTRA_CLANG_ARGS --sysroot=(xcrun --show-sdk-path)

function bup --description "Updates, upgrades and cleans Homebrew"
    brew update
    brew upgrade
    brew cleanup
end
