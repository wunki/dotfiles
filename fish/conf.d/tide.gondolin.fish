# Tide prompt layout and gondolin/flexoki colors
if status is-interactive
    # Prompt structure
    set -U tide_left_prompt_items pwd git newline character

    # zmx session indicator
    set -U tide_zmx_bg_color normal
    set -U tide_zmx_color cyan
    set -U tide_right_prompt_items zmx status cmd_duration context jobs direnv bun node python rustc java php pulumi ruby go kubectl distrobox toolbox terraform aws nix_shell crystal elixir zig

    # Colors
    gondolin_apply
end
