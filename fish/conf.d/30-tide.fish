# Tide prompt layout and Cendre colors
if status is-interactive
    set -U tide_left_prompt_items context pwd git newline character
    set -U tide_right_prompt_items status cmd_duration jobs pulumi kubectl distrobox toolbox terraform aws nix_shell

    set -U tide_character_icon λ
    set -U tide_character_vi_icon_default λ
    set -U tide_character_vi_icon_replace ▶
    set -U tide_character_vi_icon_visual V

    # Match the semantic roles in Cendre's Starship extra.
    set -U tide_character_color 73665b
    set -U tide_character_color_failure d25780

    set -U tide_pwd_color_anchors ea9875
    set -U tide_pwd_color_dirs 73665b
    set -U tide_pwd_color_truncated_dirs 73665b

    set -U tide_git_color_branch d1766e
    set -U tide_git_color_conflicted d25780
    set -U tide_git_color_dirty f4a21c
    set -U tide_git_color_operation d25780
    set -U tide_git_color_staged 43b16a
    set -U tide_git_color_stash 4e89a2
    set -U tide_git_color_untracked 58bdff
    set -U tide_git_color_upstream 4e89a2

    set -U tide_status_color 43b16a
    set -U tide_status_color_failure d25780
    set -U tide_cmd_duration_color 73665b
    set -U tide_jobs_color f4a21c
    set -U tide_time_color 73665b

    set -U tide_context_color_default a09384
    set -U tide_context_color_root d25780
    set -U tide_context_color_ssh 58bdff

    set -U tide_vi_mode_color_default 99af6b
    set -U tide_vi_mode_color_insert ea9875
    set -U tide_vi_mode_color_replace d25780
    set -U tide_vi_mode_color_visual fcba81

    set -U tide_bun_color 99af6b
    set -U tide_node_color 99af6b
    set -U tide_python_color fcba81
    set -U tide_rustc_color d1766e
    set -U tide_java_color d1766e
    set -U tide_php_color 4e89a2
    set -U tide_pulumi_color f4a21c
    set -U tide_ruby_color d1766e
    set -U tide_go_color 4e89a2
    set -U tide_gcloud_color 58bdff
    set -U tide_kubectl_color 58bdff
    set -U tide_aws_color fcba81
    set -U tide_nix_shell_color 4e89a2
    set -U tide_terraform_color 9480ba
    set -U tide_elixir_color 9480ba
    set -U tide_zig_color fcba81
    set -U tide_docker_color 58bdff
    set -U tide_distrobox_color 9480ba
    set -U tide_toolbox_color 9480ba
    set -U tide_crystal_color e6d5c2
    set -U tide_shlvl_color a09384
    set -U tide_os_color e6d5c2
    set -U tide_private_mode_color e6d5c2
    set -U tide_zmx_color 4e89a2
end
