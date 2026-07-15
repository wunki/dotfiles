# Use the active terminal palette so fzf follows the system appearance.
if status is-interactive; and type -q fzf
    set -gx FZF_DEFAULT_OPTS --color=base16
    set -g fzf_history_opts \
        --color=bg:-1,bg+:-1,fg:-1,fg+:-1,hl:magenta,hl+:bright-magenta \
        --color=header:yellow,info:blue,pointer:blue \
        --color=marker:green,prompt:red,spinner:blue
end
