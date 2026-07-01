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
    echo "Theme: $mode"
end

function __theme_sync_set_universal --argument-names name value
    set --erase --global $name
    set --universal $name $value
end
