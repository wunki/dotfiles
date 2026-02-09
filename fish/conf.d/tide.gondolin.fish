# Gondolin palette for Tide prompt colors
if status is-interactive; and type -q tide
    set -U tide_character_color FFE878
    set -U tide_character_color_failure FF7A77
    set -U tide_character_icon "λ"

    set -U tide_pwd_color_anchors 83AEF8
    set -U tide_pwd_color_dirs A6C6FE
    set -U tide_pwd_color_truncated_dirs 6A7587

    set -U tide_git_color_branch 83AEF8
    set -U tide_git_color_conflicted FF7A77
    set -U tide_git_color_dirty FFE878
    set -U tide_git_color_operation FFBE78
    set -U tide_git_color_staged B9EC86
    set -U tide_git_color_stash 83AEF8
    set -U tide_git_color_untracked B9EC86
    set -U tide_git_color_upstream 83AEF8

    set -U tide_status_color B9EC86
    set -U tide_status_color_failure FF7A77
    set -U tide_cmd_duration_color A2AEC0
    set -U tide_jobs_color FFE878
    set -U tide_time_color A2AEC0

    set -U tide_context_color_default A2AEC0
    set -U tide_context_color_root FF7A77
    set -U tide_context_color_ssh 83AEF8

    set -U tide_direnv_color FFE878
    set -U tide_direnv_color_denied FF7A77

    set -U tide_vi_mode_color_default 83AEF8
    set -U tide_vi_mode_color_insert B9EC86
    set -U tide_vi_mode_color_replace FF7A77
    set -U tide_vi_mode_color_visual CC95FF

    set -U tide_bun_color A6C6FE
    set -U tide_node_color B9EC86
    set -U tide_python_color A9E1EB
    set -U tide_rustc_color FFBE78
    set -U tide_java_color FFBE78
    set -U tide_php_color A6C6FE
    set -U tide_pulumi_color FFE878
    set -U tide_ruby_color FF7A77
    set -U tide_go_color 83AEF8
    set -U tide_gcloud_color 83AEF8
    set -U tide_kubectl_color 83AEF8
    set -U tide_aws_color FFE878
    set -U tide_nix_shell_color A6C6FE
    set -U tide_terraform_color CC95FF
    set -U tide_elixir_color CC95FF
    set -U tide_zig_color FFBE78
end
