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

    set -U tide_character_icon λ

    if test "$mode" = dark
        # --- Gondolin (dark) palette ---
        set -U tide_character_color FFE878
        set -U tide_character_color_failure FF7A77

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

        if type -q fzf
            set -g fzf_history_opts \
                --color=bg:-1,bg+:#1a1d23,fg:-1,fg+:#D0D3DA,hl:#CC95FF,hl+:#eebcbc \
                --color=header:#FFBE78,info:#83AEF8,pointer:#83AEF8 \
                --color=marker:#83AEF8,prompt:#FF7A77,spinner:#83AEF8
        end
    else
        # --- Gondolin (light) palette ---
        set -U tide_character_color aa5f1b
        set -U tide_character_color_failure aa544d

        set -U tide_pwd_color_anchors 46778d
        set -U tide_pwd_color_dirs 53879e
        set -U tide_pwd_color_truncated_dirs 939f91

        set -U tide_git_color_branch 46778d
        set -U tide_git_color_conflicted aa544d
        set -U tide_git_color_dirty 9b6e20
        set -U tide_git_color_operation aa5f1b
        set -U tide_git_color_staged 637827
        set -U tide_git_color_stash 46778d
        set -U tide_git_color_untracked 637827
        set -U tide_git_color_upstream 46778d

        set -U tide_status_color 637827
        set -U tide_status_color_failure aa544d
        set -U tide_cmd_duration_color 708089
        set -U tide_jobs_color 9b6e20
        set -U tide_time_color 708089

        set -U tide_context_color_default 708089
        set -U tide_context_color_root aa544d
        set -U tide_context_color_ssh 46778d

        set -U tide_direnv_color 9b6e20
        set -U tide_direnv_color_denied aa544d

        set -U tide_vi_mode_color_default 46778d
        set -U tide_vi_mode_color_insert 637827
        set -U tide_vi_mode_color_replace aa544d
        set -U tide_vi_mode_color_visual 85647f

        set -U tide_bun_color 53879e
        set -U tide_node_color 637827
        set -U tide_python_color 477f5e
        set -U tide_rustc_color aa5f1b
        set -U tide_java_color aa5f1b
        set -U tide_php_color 53879e
        set -U tide_pulumi_color 9b6e20
        set -U tide_ruby_color aa544d
        set -U tide_go_color 46778d
        set -U tide_gcloud_color 46778d
        set -U tide_kubectl_color 46778d
        set -U tide_aws_color 9b6e20
        set -U tide_nix_shell_color 53879e
        set -U tide_terraform_color 85647f
        set -U tide_elixir_color 85647f
        set -U tide_zig_color aa5f1b

        if type -q fzf
            set -g fzf_history_opts \
                --color=bg:-1,bg+:#eee4d6,fg:-1,fg+:#3c484f,hl:#85647f,hl+:#95728e \
                --color=header:#aa5f1b,info:#46778d,pointer:#46778d \
                --color=marker:#46778d,prompt:#aa544d,spinner:#46778d
        end
    end

    # Tide caches the branch/location color into _tide_location_color at init,
    # so refresh it here or the prompt keeps the stale (universal) branch color.
    functions -q _tide_cache_variables && _tide_cache_variables
end
