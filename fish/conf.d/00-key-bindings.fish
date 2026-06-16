# Pin fish_key_bindings before autopair.fish loads (00- sorts first).
# Fish 4.3 made this variable global (no longer persisted), so it's unset
# during conf.d. autopair bails when it's unset and never binds its keys.
# https://github.com/jorgebucaran/autopair.fish/issues/16
status is-interactive || exit

set --erase --universal fish_key_bindings
fish_default_key_bindings
set --global fish_key_bindings fish_default_key_bindings
