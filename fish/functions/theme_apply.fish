function theme_apply -d "Apply tide+fzf colors for current system appearance"
    if not status is-interactive
        return
    end

    # Accept override: theme_apply dark|light
    set -l mode $argv[1]
    if test -z "$mode"
        set mode (theme_mode)
    end

    # Skip if mode hasn't changed.
    if test "$mode" = "$__theme_current_mode"
        return
    end
    set -g __theme_current_mode $mode

    set -g tide_character_icon λ

    if test "$mode" = dark
        # --- Gondolin (dark) palette ---
        set -g tide_character_color FFE878
        set -g tide_character_color_failure FF7A77

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

        if type -q fzf
            set -g fzf_history_opts \
                --color=bg:-1,bg+:#1a1d23,fg:-1,fg+:#D0D3DA,hl:#CC95FF,hl+:#eebcbc \
                --color=header:#FFBE78,info:#83AEF8,pointer:#83AEF8 \
                --color=marker:#83AEF8,prompt:#FF7A77,spinner:#83AEF8
        end
    else
        # --- Melange Light palette ---
        set -g tide_character_color A06D00
        set -g tide_character_color_failure BF0021

        set -g tide_pwd_color_anchors 465AA4
        set -g tide_pwd_color_dirs 7892BD
        set -g tide_pwd_color_truncated_dirs A98A78

        set -g tide_git_color_branch 465AA4
        set -g tide_git_color_conflicted BF0021
        set -g tide_git_color_dirty A06D00
        set -g tide_git_color_operation BC5C00
        set -g tide_git_color_staged 3A684A
        set -g tide_git_color_stash 465AA4
        set -g tide_git_color_untracked 3A684A
        set -g tide_git_color_upstream 465AA4

        set -g tide_status_color 3A684A
        set -g tide_status_color_failure BF0021
        set -g tide_cmd_duration_color 7D6658
        set -g tide_jobs_color A06D00
        set -g tide_time_color 7D6658

        set -g tide_context_color_default 7D6658
        set -g tide_context_color_root BF0021
        set -g tide_context_color_ssh 465AA4

        set -g tide_direnv_color A06D00
        set -g tide_direnv_color_denied BF0021

        set -g tide_vi_mode_color_default 465AA4
        set -g tide_vi_mode_color_insert 3A684A
        set -g tide_vi_mode_color_replace BF0021
        set -g tide_vi_mode_color_visual 904180

        set -g tide_bun_color 7892BD
        set -g tide_node_color 3A684A
        set -g tide_python_color 3D6568
        set -g tide_rustc_color BC5C00
        set -g tide_java_color BC5C00
        set -g tide_php_color 7892BD
        set -g tide_pulumi_color A06D00
        set -g tide_ruby_color BF0021
        set -g tide_go_color 465AA4
        set -g tide_gcloud_color 465AA4
        set -g tide_kubectl_color 465AA4
        set -g tide_aws_color A06D00
        set -g tide_nix_shell_color 7892BD
        set -g tide_terraform_color 904180
        set -g tide_elixir_color 904180
        set -g tide_zig_color BC5C00

        if type -q fzf
            set -g fzf_history_opts \
                --color=bg:-1,bg+:#D9D3CE,fg:-1,fg+:#54433A,hl:#904180,hl+:#BE79BB \
                --color=header:#BC5C00,info:#465AA4,pointer:#465AA4 \
                --color=marker:#465AA4,prompt:#BF0021,spinner:#465AA4
        end
    end
end
