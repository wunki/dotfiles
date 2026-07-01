function theme-sync -d "Sync editor/terminal theme and refresh the current fish prompt"
    command theme-sync $argv
    set -l sync_status $status

    if test $sync_status -eq 0
        set -l mode (theme_mode)
        set -gx TMUX_THEME $mode

        switch $mode
            case dark
                set -gx COLORFGBG '15;0'
            case light
                set -gx COLORFGBG '0;15'
        end

        theme_apply $mode
    end

    return $sync_status
end
