# Match Fish syntax highlighting and completions to Cendre's Neovim roles.
if status is-interactive
    # Syntax
    set -g fish_color_normal e6d5c2
    set -g fish_color_command fcba81
    set -g fish_color_builtin fcba81
    set -g fish_color_function fcba81
    set -g fish_color_keyword d1766e
    set -g fish_color_quote 99af6b
    set -g fish_color_redirection a09384
    set -g fish_color_end a09384
    set -g fish_color_error d25780
    set -g fish_color_param e6d5c2
    set -g fish_color_option ea9875
    set -g fish_color_comment 73665b --italics
    set -g fish_color_operator a09384
    set -g fish_color_escape ea9875
    set -g fish_color_autosuggestion 73665b
    set -g fish_color_cancel d25780 --reverse
    set -g fish_color_valid_path 4e89a2 --underline

    # Command-line state
    set -g fish_color_selection e6d5c2 --background=2f1e17
    set -g fish_color_search_match 171311 --background=fcba81
    set -g fish_color_history_current --bold
    set -g fish_color_cwd 4e89a2
    set -g fish_color_cwd_root d25780
    set -g fish_color_user 99af6b
    set -g fish_color_host a09384
    set -g fish_color_host_remote 58bdff
    set -g fish_color_status d25780

    # Completion pager, mirroring Cendre's Neovim popup menu.
    set -g fish_pager_color_background --background=0f0c0a
    set -g fish_pager_color_prefix ea9875 --bold
    set -g fish_pager_color_completion e6d5c2
    set -g fish_pager_color_description a09384
    set -g fish_pager_color_progress 171311 --background=ea9875 --bold
    set -g fish_pager_color_selected_background --background=2a2422
    set -g fish_pager_color_selected_prefix ea9875 --bold
    set -g fish_pager_color_selected_completion e6d5c2 --bold
    set -g fish_pager_color_selected_description a09384 --bold
    set -g fish_pager_color_secondary_background --background=0f0c0a
end
