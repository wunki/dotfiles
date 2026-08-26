# Shell workflows backed by AI tools.

function smart-commit --description "Plan and approve atomic commits with Codex"
    for dependency in codex jq
        if not command -q $dependency
            echo "smart-commit: required command not found: $dependency" >&2
            return 127
        end
    end

    set -l repo_root (git rev-parse --show-toplevel 2>/dev/null)
    if test $status -ne 0
        echo "smart-commit: not inside a Git repository" >&2
        return 1
    end

    set -l schema "$__fish_config_dir/ai/smart-commit-plan.schema.json"
    if not test -r "$schema"
        echo "smart-commit: plan schema not found: $schema" >&2
        return 1
    end

    set -l plan_file (mktemp -t smart-commit-plan.XXXXXX)
    or return $status
    set -l log_file (mktemp -t smart-commit-log.XXXXXX)
    or begin
        rm -f -- "$plan_file"
        return $status
    end

    echo "Analyzing uncommitted changes..."

    codex --ask-for-approval never exec \
        --ephemeral \
        --cd "$repo_root" \
        --model gpt-5.6-luna \
        --config 'model_reasoning_effort="low"' \
        --config 'service_tier="fast"' \
        --enable fast_mode \
        --sandbox read-only \
        --output-schema "$schema" \
        --output-last-message "$plan_file" \
        '$smart-commit Perform the analysis and grouping steps for all uncommitted changes. Do not stage or commit anything. Return only the structured commit plan requested by the output schema. Every changed file must appear exactly once using a repository-root-relative path. Use an empty body when the skill says to omit it. Use status "clean" when there are no changes and "blocked" for conflicts or another condition that prevents committing. The calling shell handles confirmation and execution.' \
        >/dev/null 2>"$log_file"
    set -l codex_status $status

    if test $codex_status -ne 0
        echo "smart-commit: Codex analysis failed" >&2
        cat "$log_file" >&2
        rm -f -- "$plan_file" "$log_file"
        return $codex_status
    end

    if not jq -e . "$plan_file" >/dev/null 2>&1
        echo "smart-commit: Codex returned an invalid commit plan" >&2
        cat "$plan_file" >&2
        rm -f -- "$plan_file" "$log_file"
        return 1
    end

    set -l plan_status (jq -r .status "$plan_file")
    switch $plan_status
        case clean
            jq -r .message "$plan_file"
            rm -f -- "$plan_file" "$log_file"
            return 0
        case blocked
            jq -r .message "$plan_file" >&2
            rm -f -- "$plan_file" "$log_file"
            return 1
        case ready
            # Continue below.
        case '*'
            echo "smart-commit: unknown plan status: $plan_status" >&2
            rm -f -- "$plan_file" "$log_file"
            return 1
    end

    set -l commit_count (jq '.commits | length' "$plan_file")
    if test "$commit_count" -eq 0
        echo "smart-commit: Codex returned an empty commit plan" >&2
        rm -f -- "$plan_file" "$log_file"
        return 1
    end

    set -l planned_file_count (jq '[.commits[].files[]] | length' "$plan_file")
    set -l planned_files (jq -r '.commits[].files[]' "$plan_file" | sort -u)
    if test "$planned_file_count" -ne (count $planned_files)
        echo "smart-commit: a file appears in more than one proposed commit" >&2
        rm -f -- "$plan_file" "$log_file"
        return 1
    end

    set -l changed_files (
        begin
            git -C "$repo_root" diff --name-only
            git -C "$repo_root" diff --cached --name-only
            git -C "$repo_root" ls-files --others --exclude-standard
        end | sort -u
    )
    set -l planned_file_list (string join \n -- $planned_files)
    set -l changed_file_list (string join \n -- $changed_files)
    if test "$planned_file_list" != "$changed_file_list"
        echo "smart-commit: proposed files no longer match the working tree; run it again" >&2
        rm -f -- "$plan_file" "$log_file"
        return 1
    end

    set -l commit_label commit
    if test "$commit_count" -ne 1
        set commit_label commits
    end
    set -l body_width (math "min(92, max(40, $COLUMNS - 10))")

    printf '\n'
    set_color $fish_color_command --bold
    printf 'Commit plan'
    set_color normal
    set_color $fish_color_comment
    printf '  %d %s\n' "$commit_count" "$commit_label"
    set_color normal

    for index in (seq 0 (math "$commit_count - 1"))
        set -l subject (jq -r --argjson index $index '.commits[$index].subject' "$plan_file")
        set -l body (jq -r --argjson index $index '.commits[$index].body' "$plan_file")
        set -l files (jq -r --argjson index $index '.commits[$index].files[]' "$plan_file")

        printf '\n'
        set_color $fish_color_keyword --bold
        printf '%2d' (math "$index + 1")
        set_color normal
        printf '  '
        set_color $fish_color_normal --bold
        printf '%s\n' "$subject"
        set_color normal

        for file_index in (seq 1 (count $files))
            set -l connector '├─'
            if test "$file_index" -eq (count $files)
                set connector '└─'
            end

            set_color $fish_color_comment
            printf '     %s ' "$connector"
            set_color $fish_color_cwd
            printf '%s\n' "$files[$file_index]"
            set_color normal
        end

        if test -n "$body"
            printf '\n'
            printf '%s\n' "$body" | fold -s -w "$body_width" | while read -l line
                set_color $fish_color_comment
                printf '        %s\n' "$line"
                set_color normal
            end
        end
    end

    printf '\n'
    set_color $fish_color_option --bold
    printf 'Commit all %d %s?' "$commit_count" "$commit_label"
    set_color normal
    read --local --prompt-str " [y/N] " approval
    or begin
        rm -f -- "$plan_file" "$log_file"
        return 130
    end

    if not contains -- (string lower -- "$approval") y yes
        echo "Commit plan rejected; nothing was committed."
        rm -f -- "$plan_file" "$log_file"
        return 1
    end

    for index in (seq 0 (math "$commit_count - 1"))
        set -l subject (jq -r --argjson index $index '.commits[$index].subject' "$plan_file")
        set -l body (jq -r --argjson index $index '.commits[$index].body' "$plan_file")
        set -l files (jq -r --argjson index $index '.commits[$index].files[]' "$plan_file")

        for file in $files
            if string match -qr '(^/|(^|/)\.\.(/|$))' -- "$file"
                echo "smart-commit: unsafe path in commit plan: $file" >&2
                rm -f -- "$plan_file" "$log_file"
                return 1
            end

            if test -z "$(git -C "$repo_root" status --short -- "$file")"
                echo "smart-commit: planned file is no longer changed: $file" >&2
                rm -f -- "$plan_file" "$log_file"
                return 1
            end
        end

        git -C "$repo_root" add -A -- $files
        or begin
            rm -f -- "$plan_file" "$log_file"
            return $status
        end

        if test -n "$body"
            git -C "$repo_root" commit --only -m "$subject" -m "$body" -- $files >"$log_file" 2>&1
        else
            git -C "$repo_root" commit --only -m "$subject" -- $files >"$log_file" 2>&1
        end
        set -l commit_status $status
        if test "$commit_status" -ne 0
            cat "$log_file" >&2
            rm -f -- "$plan_file" "$log_file"
            return $commit_status
        end

        set_color $fish_color_quote
        printf '✓ '
        set_color normal
        printf '%s\n' "$subject"
    end

    rm -f -- "$plan_file" "$log_file"
    set_color $fish_color_quote --bold
    printf '\nCreated %d %s\n' "$commit_count" "$commit_label"
    set_color normal
    for entry in (git -C "$repo_root" log --format='%h%x09%s' -"$commit_count")
        set -l fields (string split -m 1 \t -- "$entry")
        set_color $fish_color_quote
        printf '%s' "$fields[1]"
        set_color normal
        printf '  %s\n' "$fields[2]"
    end
end
