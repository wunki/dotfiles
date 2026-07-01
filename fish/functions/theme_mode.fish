function theme_mode -d "Print manually selected appearance: dark or light"
    if set -q theme_mode_override; and contains -- $theme_mode_override dark light
        echo $theme_mode_override
        return 0
    end

    if set -q TMUX_THEME; and contains -- $TMUX_THEME dark light
        echo $TMUX_THEME
        return 0
    end

    # COLORFGBG's last field is the terminal background color when provided by
    # the terminal or parent environment.
    if set -q COLORFGBG
        set -l colorfgbg_parts (string split ';' -- $COLORFGBG)
        set -l bg $colorfgbg_parts[-1]
        switch $bg
            case 0 8
                echo dark
                return 0
            case 7 15
                echo light
                return 0
        end
    end

    if set -q theme_mode_fallback; and contains -- $theme_mode_fallback dark light
        echo $theme_mode_fallback
    else
        echo dark
    end
end
