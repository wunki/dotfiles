# Emacs vterm integration. Pass its escape sequences through terminal
# multiplexers before they reach Emacs.
function vterm_printf;
    if begin; [  -n "$TMUX" ]  ; and  string match -q -r "screen|tmux" "$TERM"; end 
        # tmux requires an extra escape layer.
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end

function clear
    vterm_printf "51;Evterm-clear-scrollback";
    tput clear;
end

function vterm_prompt_end;
    vterm_printf '51;A'(whoami)'@'(hostname)':'(pwd)
end
functions --copy fish_prompt vterm_old_fish_prompt

function fish_prompt --description 'Write out the prompt; do not replace this. Instead, put this at end of your file.'
    # Remove the original prompt's trailing newline while preserving escape codes.
    printf "%b" (string join "\n" (vterm_old_fish_prompt))
    vterm_prompt_end
end
