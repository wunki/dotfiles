# Tide prompt layout
if status is-interactive
    set -U tide_left_prompt_items pwd git newline character
    set -U tide_right_prompt_items status cmd_duration context jobs pulumi kubectl distrobox toolbox terraform aws nix_shell

    set -U tide_character_icon λ
    set -U tide_character_vi_icon_default λ
    set -U tide_character_vi_icon_replace ▶
    set -U tide_character_vi_icon_visual V
end
