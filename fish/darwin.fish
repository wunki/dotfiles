set -x PROJECT_DIR {$HOME}/Code
set -x OPENSSL_INCLUDE_DIR /usr/local/opt/openssl/include
set -x OPENSSL_LIB /usr/local/opt/openssl/lib
set -x OPENSSL_ROOT_DIR /usr/local/opt/openssl

function bup --description "Updates, upgrades and cleanes Homebrew"
    brew update
    brew upgrade
    brew cleanup
end

# Emacs on the Mac
if test -d "/Applications/Emacs.app"
    abbr emacs '/Applications/Emacs.app/Contents/MacOS/Emacs -nw'
end

prepend_to_path "/Applications/Emacs.app/Contents/MacOS/bin"
prepend_to_path /usr/local/opt/go/libexec/bin
prepend_to_path /usr/local/opt/mono/bin
prepend_to_path "/Applications/Postgres.app/Contents/Versions/11.0/bin"
prepend_to_path /usr/local/Cellar/emacs/HEAD/bin
prepend_to_path "/Applications/Postgres.app/Contents/Versions/latest/bin"
