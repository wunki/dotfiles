# Tide prompt layout and auto-switching theme colors
if status is-interactive
    # Prompt structure
    set -U tide_left_prompt_items pwd git newline character

    # zmx session indicator
    set -U tide_zmx_bg_color normal
    set -U tide_zmx_color cyan
    set -U tide_right_prompt_items zmx status cmd_duration context jobs direnv pulumi kubectl distrobox toolbox terraform aws nix_shell

    # Apply colors on startup
    theme_apply

    # Re-check appearance periodically; querying the desktop portal costs a few ms.
    function __theme_check --on-event fish_prompt
        set -q theme_check_interval; or set -g theme_check_interval 10
        set -q __theme_check_count; or set -g __theme_check_count 0

        set -g __theme_check_count (math $__theme_check_count + 1)
        if test $__theme_check_count -ge $theme_check_interval
            set -g __theme_check_count 0
            theme_apply
        end
    end
end
