# Vite+ bin (https://viteplus.dev)
# Keep startup cheap: define the wrapper here and lazy-load completions from
# completions/vp.fish + completions/vpr.fish instead of running `vp` at startup.
fish_add_path -pP "$HOME/.vite-plus/bin"

function vp
    if test (count $argv) -ge 2; and test "$argv[1]" = env; and test "$argv[2]" = use
        if contains -- -h $argv; or contains -- --help $argv
            command vp $argv
            return
        end

        set -lx VP_ENV_USE_EVAL_ENABLE 1
        set -lx VP_SHELL fish
        set -l __vp_out (command vp $argv); or return $status
        eval (string join ';' $__vp_out)
    else
        command vp $argv
    end
end
