# OpenSSL
set -x OPENSSL_INCLUDE_DIR /usr/local/opt/openssl/include
set -x OPENSSL_LIB /usr/local/opt/openssl/lib
set -x OPENSSL_ROOT_DIR /usr/local/opt/openssl

# Environment variables
prepend_to_path "/usr/local/Cellar/emacs/HEAD/bin"
prepend_to_path "/Applications/Postgres.app/Contents/Versions/latest/bin"