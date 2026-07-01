function theme-sync -d "Sync the fish prompt theme"
    set -l mode $argv[1]
    if test -z "$mode"
        set mode toggle
    end

    switch $mode
        case dark light
            # Valid explicit mode.
        case toggle
            if test (theme_mode) = light
                set mode dark
            else
                set mode light
            end
        case '*'
            echo "usage: theme-sync [dark|light|toggle]" >&2
            return 2
    end

    __theme_sync_set_universal theme_mode_override $mode
    __theme_sync_set_universal theme_mode_fallback $mode

    theme_apply --force $mode
    __theme_sync_notify_nvim $mode
    echo "Theme: $mode"
end

function __theme_sync_notify_nvim --argument-names mode
    type -q nvim; or return 0

    set -l cmd GondolinDark
    test "$mode" = light; and set cmd GondolinLight

    # Every running Neovim listens on a default server socket under
    # /tmp/nvim.$USER. Push the matching Gondolin command into each one;
    # stale sockets from crashed instances just fail silently.
    for sock in /tmp/nvim.$USER/*/nvim.*.0
        nvim --server $sock --remote-expr "execute('$cmd')" >/dev/null 2>&1
    end
end

function __theme_sync_set_universal --argument-names name value
    set --erase --global $name
    set --universal $name $value
end
