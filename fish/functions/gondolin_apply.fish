function gondolin_apply -d "Apply tide+fzf colors for current macOS appearance"
    if not status is-interactive
        return
    end

    # Accept override: gondolin_apply dark|light
    set -l mode $argv[1]
    if test -z "$mode"
        set mode dark
        if type -q defaults
            if defaults read -g AppleInterfaceStyle &>/dev/null
                set mode dark
            else
                set mode light
            end
        end
    end

    set -U tide_character_icon "λ"

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
        # --- Flexoki Light palette ---
        set -U tide_character_color AD8301
        set -U tide_character_color_failure AF3029

        set -U tide_pwd_color_anchors 205EA6
        set -U tide_pwd_color_dirs 4385BE
        set -U tide_pwd_color_truncated_dirs 878580

        set -U tide_git_color_branch 205EA6
        set -U tide_git_color_conflicted AF3029
        set -U tide_git_color_dirty AD8301
        set -U tide_git_color_operation BC5215
        set -U tide_git_color_staged 66800B
        set -U tide_git_color_stash 205EA6
        set -U tide_git_color_untracked 66800B
        set -U tide_git_color_upstream 205EA6

        set -U tide_status_color 66800B
        set -U tide_status_color_failure AF3029
        set -U tide_cmd_duration_color 6F6E69
        set -U tide_jobs_color AD8301
        set -U tide_time_color 6F6E69

        set -U tide_context_color_default 6F6E69
        set -U tide_context_color_root AF3029
        set -U tide_context_color_ssh 205EA6

        set -U tide_direnv_color AD8301
        set -U tide_direnv_color_denied AF3029

        set -U tide_vi_mode_color_default 205EA6
        set -U tide_vi_mode_color_insert 66800B
        set -U tide_vi_mode_color_replace AF3029
        set -U tide_vi_mode_color_visual 5E409D

        set -U tide_bun_color 4385BE
        set -U tide_node_color 66800B
        set -U tide_python_color 24837B
        set -U tide_rustc_color BC5215
        set -U tide_java_color BC5215
        set -U tide_php_color 4385BE
        set -U tide_pulumi_color AD8301
        set -U tide_ruby_color AF3029
        set -U tide_go_color 205EA6
        set -U tide_gcloud_color 205EA6
        set -U tide_kubectl_color 205EA6
        set -U tide_aws_color AD8301
        set -U tide_nix_shell_color 4385BE
        set -U tide_terraform_color 5E409D
        set -U tide_elixir_color 5E409D
        set -U tide_zig_color BC5215

        if type -q fzf
            set -g fzf_history_opts \
                --color=bg:-1,bg+:#E6E4D9,fg:-1,fg+:#100F0F,hl:#5E409D,hl+:#8B7EC8 \
                --color=header:#BC5215,info:#205EA6,pointer:#205EA6 \
                --color=marker:#205EA6,prompt:#AF3029,spinner:#205EA6
        end
    end
end
