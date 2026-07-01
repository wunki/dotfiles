function theme_mode -d "Print current system appearance: dark or light"
    if set -q theme_mode_override; and contains -- $theme_mode_override dark light
        echo $theme_mode_override
        return 0
    end

    if set -q TMUX_THEME; and contains -- $TMUX_THEME dark light
        echo $TMUX_THEME
        return 0
    end

    # Match Pi's final fallback: COLORFGBG's last field is the terminal
    # background color. theme-sync sets 15;0 for dark and 0;15 for light.
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

    set -l fallback dark
    if set -q theme_mode_fallback; and contains -- $theme_mode_fallback dark light
        set fallback $theme_mode_fallback
    end

    switch (uname)
        case Darwin
            if type -q defaults
                if defaults read -g AppleInterfaceStyle >/dev/null 2>/dev/null
                    echo dark
                else
                    echo light
                end
                return 0
            end

        case Linux
            if type -q dbus-send
                set -l response (dbus-send \
                    --session \
                    --print-reply=literal \
                    --reply-timeout=200 \
                    --dest=org.freedesktop.portal.Desktop \
                    /org/freedesktop/portal/desktop \
                    org.freedesktop.portal.Settings.Read \
                    string:org.freedesktop.appearance \
                    string:color-scheme 2>/dev/null)

                if test $status -eq 0
                    # freedesktop portal: 0 = no preference, 1 = dark, 2 = light
                    if string match -qr "uint32 1" -- $response
                        echo dark
                        return 0
                    else if string match -qr "uint32 [02]" -- $response
                        echo light
                        return 0
                    end
                end
            end

            if type -q gsettings
                set -l color_scheme (gsettings get org.gnome.desktop.interface color-scheme 2>/dev/null)
                set -l gsettings_status $status
                if string match -qi "*dark*" -- $color_scheme
                    echo dark
                    return 0
                else if test $gsettings_status -eq 0
                    echo light
                    return 0
                end
            end
    end

    echo $fallback
end
