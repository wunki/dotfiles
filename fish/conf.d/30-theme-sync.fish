# Stale env copies from long-lived parents (terminal app, tmux server) shadow
# the universal source of truth, reverting new sessions to the old mode.
for name in theme_mode_override theme_mode_fallback
    set --erase --global $name
end

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
    __theme_sync_notify_pi $mode
    __theme_sync_notify_claude $mode
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

function __theme_sync_notify_pi --argument-names mode
    set -l themes_dir ~/.pi/agent/themes
    set -l src $themes_dir/gondolin-$mode.json
    test -f $src; or return 0

    # Pi's settings point at the generated "gondolin" theme. Pi watches the
    # active theme file and hot-reloads it, so rewriting gondolin.json restyles
    # every running pi session.
    string replace '"name": "gondolin-'$mode'"' '"name": "gondolin"' <$src >$themes_dir/gondolin.json
end

function __theme_sync_notify_claude --argument-names mode
    set -l settings ~/.claude/settings.json
    test -f $settings; or return 0
    type -q jq; or return 0

    # Claude Code watches settings.json and hot-reloads the theme field in
    # running sessions. Rewrite in place so the watcher keeps a stable inode.
    set -l updated (jq --arg theme $mode '.theme = $theme' $settings)
    test -n "$updated"; and printf '%s\n' $updated >$settings
end

function __theme_sync_set_universal --argument-names name value
    set --erase --global $name
    # --unexport: exported universals leak into long-lived parent environments,
    # and fish preserves the export flag on plain re-set -U.
    set --universal --unexport $name $value
end
