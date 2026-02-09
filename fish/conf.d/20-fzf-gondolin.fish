if status is-interactive; and type -q fzf
    set -g fzf_history_opts \
        --color=bg:-1,bg+:#1a1d23,fg:-1,fg+:#D0D3DA,hl:#CC95FF,hl+:#eebcbc \
        --color=header:#FFBE78,info:#83AEF8,pointer:#83AEF8 \
        --color=marker:#83AEF8,prompt:#FF7A77,spinner:#83AEF8
end
