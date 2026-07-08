function cpu --description "Show a pretty snapshot of processes using the most CPU"
    argparse 'h/help' 'l/live' -- $argv
    or return 2

    if set -q _flag_help
        echo "Usage: cpu [count]"
        echo "       cpu --live"
        echo
        echo "Show a pretty snapshot of the top CPU-consuming processes."
        echo "count defaults to 15. Use --live for btop/top."
        return 0
    end

    if set -q _flag_live
        if type -q btop
            command btop
        else
            switch (uname -s)
                case Darwin FreeBSD
                    command top -o cpu
                case Linux
                    command top -o %CPU
                case '*'
                    command top
            end
        end
        return $status
    end

    set -l limit 15
    if test (count $argv) -gt 0
        set limit $argv[1]
    end

    if not string match -qr '^[1-9][0-9]*$' -- $limit
        echo "cpu: count must be a positive integer" >&2
        return 2
    end

    set -l dim (set_color brblack)
    set -l bold (set_color --bold)
    set -l reset (set_color normal)
    set -l green (set_color green)
    set -l yellow (set_color yellow)
    set -l red (set_color red)
    set -l cyan (set_color cyan)
    set -l bar_width 18
    set -l elapsed_width 12
    set -l fixed_width (math "2 + 7 + 2 + $bar_width + 2 + 6 + 2 + $elapsed_width + 2 + 7")
    set -l term_cols (command tput cols 2>/dev/null)
    if not string match -qr '^[1-9][0-9]*$' -- $term_cols
        set term_cols 100
    end
    set -l name_width (math -s0 "max(28, min(52, $term_cols - $fixed_width))")
    set -l table_width (math $fixed_width + $name_width)

    printf "%s%s%s\n" $dim (string repeat -n $table_width "─") $reset
    printf "%sCPU snapshot%s %s(one core = 100%%; sorted by current CPU)\n" $bold $reset $dim
    printf "%s%-*s  %7s  %-18s  %6s  %12s  %7s%s\n" $cyan $name_width "PROCESS" "CPU" "LOAD" "MEM" "ELAPSED" "PID" $reset

    set -l rows
    switch (uname -s)
        case Darwin FreeBSD
            set rows (command ps -axo pid=,ppid=,pcpu=,pmem=,stat=,etime=,comm= -r | command head -n $limit)
        case Linux
            set rows (command ps -eo pid=,ppid=,pcpu=,pmem=,stat=,etime=,comm= --sort=-pcpu | command head -n $limit)
        case '*'
            set rows (command ps -axo pid=,ppid=,pcpu=,pmem=,stat=,etime=,comm= | command sort -nrk 3 | command head -n $limit)
    end

    for row in $rows
        set -l fields (string split -n " " -- (string trim -- $row))
        if test (count $fields) -lt 7
            continue
        end

        set -l pid $fields[1]
        set -l cpu_percent $fields[3]
        set -l mem_percent $fields[4]
        set -l elapsed $fields[6]
        set -l command_path (string join " " -- $fields[7..-1])
        set -l process_name (command basename -- "$command_path" 2>/dev/null)
        if test -z "$process_name"
            set process_name $command_path
        end
        set process_name (string shorten -m $name_width -- $process_name)

        set -l color $green
        if test $cpu_percent -ge 75
            set color $red
        else if test $cpu_percent -ge 35
            set color $yellow
        end

        set -l filled (math -s0 "min($cpu_percent, 100) * $bar_width / 100")
        if test $filled -lt 0
            set filled 0
        else if test $filled -eq 0; and not string match -qr '^0+(\.0+)?$' -- $cpu_percent
            set filled 1
        else if test $filled -gt $bar_width
            set filled $bar_width
        end
        set -l empty (math $bar_width - $filled)
        set -l filled_bar (string repeat -n $filled "█")
        set -l empty_bar (string repeat -n $empty "░")

        printf "%-*s  %s%6.1f%%%s  %s%s%s%s%s  %5.1f%%  %12s  %7s\n" \
            $name_width $process_name \
            $color $cpu_percent $reset \
            $color "$filled_bar" $dim "$empty_bar" $reset \
            $mem_percent $elapsed $pid
    end

    printf "%s%s%s\n" $dim (string repeat -n $table_width "─") $reset
end
