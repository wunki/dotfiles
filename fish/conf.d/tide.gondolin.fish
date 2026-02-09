# Gondolin palette for Tide prompt colors
if status is-interactive; and type -q tide
    set -g tide_character_color FFE878
    set -g tide_character_color_failure FF7A77
    set -g tide_character_icon "λ"

    set -g tide_pwd_color_anchors 83AEF8
    set -g tide_pwd_color_dirs A6C6FE
    set -g tide_pwd_color_truncated_dirs 6A7587

    set -g tide_git_color_branch 83AEF8
    set -g tide_git_color_conflicted FF7A77
    set -g tide_git_color_dirty FFE878
    set -g tide_git_color_operation FFBE78
    set -g tide_git_color_staged B9EC86
    set -g tide_git_color_stash 83AEF8
    set -g tide_git_color_untracked B9EC86
    set -g tide_git_color_upstream 83AEF8

    set -g tide_status_color B9EC86
    set -g tide_status_color_failure FF7A77
    set -g tide_cmd_duration_color A2AEC0
    set -g tide_jobs_color FFE878
    set -g tide_time_color A2AEC0

    set -g tide_context_color_default A2AEC0
    set -g tide_context_color_root FF7A77
    set -g tide_context_color_ssh 83AEF8

    set -g tide_direnv_color FFE878
    set -g tide_direnv_color_denied FF7A77

    set -g tide_vi_mode_color_default 83AEF8
    set -g tide_vi_mode_color_insert B9EC86
    set -g tide_vi_mode_color_replace FF7A77
    set -g tide_vi_mode_color_visual CC95FF

    set -g tide_bun_color A6C6FE
    set -g tide_node_color B9EC86
    set -g tide_python_color A9E1EB
    set -g tide_rustc_color FFBE78
    set -g tide_java_color FFBE78
    set -g tide_php_color A6C6FE
    set -g tide_pulumi_color FFE878
    set -g tide_ruby_color FF7A77
    set -g tide_go_color 83AEF8
    set -g tide_gcloud_color 83AEF8
    set -g tide_kubectl_color 83AEF8
    set -g tide_aws_color FFE878
    set -g tide_nix_shell_color A6C6FE
    set -g tide_terraform_color CC95FF
    set -g tide_elixir_color CC95FF
    set -g tide_zig_color FFBE78
end
