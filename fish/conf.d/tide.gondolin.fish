# Tide prompt layout and auto-switching theme colors
if status is-interactive
    # Prompt structure
    set -g tide_left_prompt_items pwd git newline character

    set -g tide_right_prompt_items status cmd_duration context jobs direnv pulumi kubectl distrobox toolbox terraform aws nix_shell

    # Apply colors on startup
    theme_apply

    # Re-apply on each prompt so externally-run `theme-sync` updates existing shells.
    # This is cheap because theme_mode is manual-only and theme_apply returns when unchanged.
    function __theme_check --on-event fish_prompt
        theme_apply
    end
end
