function __vpr_complete
    set -l tokens (commandline --current-process --tokenize --cut-at-cursor)
    set -l current (commandline --current-token)
    VP_COMPLETE=fish command vp -- vp run $tokens[2..] $current
end

complete -c vpr --keep-order --exclusive --arguments "(__vpr_complete)"
