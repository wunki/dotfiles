# Match fzf and fzf.fish to the Cendre hard palette.
if status is-interactive; and type -q fzf; and test -r ~/.config/fzf/cendre.fish
    source ~/.config/fzf/cendre.fish
    set -g fzf_history_opts $FZF_DEFAULT_OPTS
end
