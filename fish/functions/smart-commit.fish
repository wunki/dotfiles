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
    set -l message_file (mktemp -t smart-commit-message.XXXXXX)
    or begin
        rm -f -- "$plan_file" "$log_file"
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
        '$petar-atomic-commits Perform the analysis and grouping steps for all uncommitted changes. Do not stage or commit anything. Return only the structured commit plan requested by the output schema. Every changed file must appear exactly once using a repository-root-relative path. Use an empty body when the skill says to omit it. Use status "clean" when there are no changes and "blocked" for conflicts or another condition that prevents committing. The calling shell handles confirmation and execution.' \
        >/dev/null 2>"$log_file"
    set -l codex_status $status

    if test $codex_status -ne 0
        echo "smart-commit: Codex analysis failed" >&2
        cat "$log_file" >&2
        rm -f -- "$plan_file" "$log_file" "$message_file"
        return $codex_status
    end

    if not jq -e . "$plan_file" >/dev/null 2>&1
        echo "smart-commit: Codex returned an invalid commit plan" >&2
        cat "$plan_file" >&2
        rm -f -- "$plan_file" "$log_file" "$message_file"
        return 1
    end

    set -l plan_status (jq -r .status "$plan_file")
    switch $plan_status
        case clean
            jq -r .message "$plan_file"
            rm -f -- "$plan_file" "$log_file" "$message_file"
            return 0
        case blocked
            jq -r .message "$plan_file" >&2
            rm -f -- "$plan_file" "$log_file" "$message_file"
            return 1
        case ready
            # Continue below.
        case '*'
            echo "smart-commit: unknown plan status: $plan_status" >&2
            rm -f -- "$plan_file" "$log_file" "$message_file"
            return 1
    end

    set -l commit_count (jq '.commits | length' "$plan_file")
    if test "$commit_count" -eq 0
        echo "smart-commit: Codex returned an empty commit plan" >&2
        rm -f -- "$plan_file" "$log_file" "$message_file"
        return 1
    end

    set -l planned_file_count (jq '[.commits[].files[]] | length' "$plan_file")
    set -l planned_files (jq -r '.commits[].files[]' "$plan_file" | sort -u)
    if test "$planned_file_count" -ne (count $planned_files)
        echo "smart-commit: a file appears in more than one proposed commit" >&2
        rm -f -- "$plan_file" "$log_file" "$message_file"
        return 1
    end

    # --no-renames lists a staged rename as its old and new path, matching a
    # plan that names both; without it Git shows only the new path. -z prints
    # each name as it is: otherwise Git quotes names with non-ASCII letters,
    # tabs or quotes ("caf\303\251.md"), and they never match the plan.
    set -l changed_files (
        begin
            git -C "$repo_root" diff --name-only --no-renames -z | string split0
            git -C "$repo_root" diff --cached --name-only --no-renames -z | string split0
            git -C "$repo_root" ls-files --others --exclude-standard -z | string split0
        end | sort -u
    )

    # git add or git commit --only fails on these changes, often after
    # earlier commits in the plan have landed. Stop before anything is
    # committed and say what to do by hand. This runs before the plan check
    # below so the reason shows whether or not the plan lists the file.
    set -l staged_added (git -C "$repo_root" diff --cached --name-only --no-renames -z --diff-filter=A | string split0)
    set -l staged_deleted (git -C "$repo_root" diff --cached --name-only --no-renames -z --diff-filter=D | string split0)
    set -l worktree_deleted (git -C "$repo_root" diff --name-only --no-renames -z --diff-filter=D | string split0)
    set -l ignore_case (git -C "$repo_root" config --type=bool core.ignorecase)
    set -l blockers

    # On a case-insensitive file system Git treats both names of a
    # case-only rename (Readme.md to README.md) as one file, and
    # git commit --only refuses the pair. Sorting the changed paths by their
    # lowercased name puts the two names next to each other, so one pass
    # finds them; comparing every pair took minutes on large changes. Each
    # key is escaped so a name with a newline stays on one line, and carries
    # its index into $changed_files to get the name back.
    set -l case_renamed
    if test "$ignore_case" = true; and set -q changed_files[2]
        set -l keyed
        for index in (seq (count $changed_files))
            set -a keyed (string escape -- $changed_files[$index] | string lower)\t$index
        end
        set -l previous_key
        set -l previous_index
        # The C locale sorts byte by byte; other locales can skip the tab
        # and split a pair.
        for line in (printf '%s\n' $keyed | LC_ALL=C sort)
            set -l key_index (string split -r -m 1 \t -- $line)
            if test "$key_index[1]" = "$previous_key"
                set -l old_name $changed_files[$previous_index]
                set -l new_name $changed_files[$key_index[2]]
                if contains -- $new_name $staged_deleted
                    set old_name $changed_files[$key_index[2]]
                    set new_name $changed_files[$previous_index]
                end
                if contains -- $old_name $staged_deleted
                    set -a case_renamed $old_name
                    set -a blockers "renaming $old_name to $new_name only changes case, which smart-commit cannot commit here; commit it by hand and try again"
                end
            end
            set previous_key $key_index[1]
            set previous_index $key_index[2]
        end
    end

    for file in $changed_files
        # Staged as new, then deleted from disk: no change is left to commit,
        # and git commit --only rejects a path it cannot find anywhere.
        if contains -- $file $staged_added; and contains -- $file $worktree_deleted
            set -a blockers "$file was staged as new and then deleted; run git rm --cached -- "(string escape -- $file)" and try again"
        end

        if not contains -- $file $staged_deleted; or contains -- $file $case_renamed
            continue
        end

        # Deleted with git rm and replaced by a directory of the same name.
        # git commit --only cannot read a directory where the file was.
        set -l disk_path "$repo_root/$file"
        if test -d "$disk_path"; and not test -L "$disk_path"
            set -a blockers "$file was replaced by a directory, which smart-commit cannot commit; commit it by hand and try again"
            continue
        end

        # A file removed from the index with git rm --cached but kept on
        # disk. git commit --only reads each path from disk, so it would add
        # the file back and undo the untracking. The index looks the same
        # after deleting a file and writing a new one in its place (git rm
        # foo, then a new foo; git mv a b, then a new a), which commits
        # fine. So stop only when the file still matches the last commit or
        # is ignored now; an untracked file edited since goes through as a
        # rewrite.
        set -l disk_blob
        if test -L "$disk_path"
            # git hash-object would hash the link's target; Git stores the
            # link text.
            set disk_blob (printf '%s' (readlink -- "$disk_path") | git -C "$repo_root" hash-object --stdin)
        else if test -f "$disk_path"
            set disk_blob (git -C "$repo_root" hash-object -- "$file")
        else
            continue
        end
        if test "$disk_blob" = "$(git -C "$repo_root" rev-parse -q --verify "HEAD:$file")"
            or git -C "$repo_root" check-ignore -q -- "$file"
            set -a blockers "$file is untracked with git rm --cached, which smart-commit cannot commit; commit it by hand and try again"
        end
    end
    if set -q blockers[1]
        printf 'smart-commit: %s\n' $blockers >&2
        echo "Nothing was committed." >&2
        rm -f -- "$plan_file" "$log_file" "$message_file"
        return 1
    end

    set -l planned_file_list (string join \n -- $planned_files)
    set -l changed_file_list (string join \n -- $changed_files)
    if test "$planned_file_list" != "$changed_file_list"
        echo "smart-commit: proposed files no longer match the working tree; run it again" >&2
        rm -f -- "$plan_file" "$log_file" "$message_file"
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
            # Unquoted, so each line of the body (one list item each) prints
            # on its own line and paragraph breaks survive.
            printf '%s\n' $body | fold -s -w "$body_width" | while read -l line
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
        rm -f -- "$plan_file" "$log_file" "$message_file"
        return 130
    end

    if not contains -- (string lower -- "$approval") y yes
        echo "Commit plan rejected; nothing was committed."
        rm -f -- "$plan_file" "$log_file" "$message_file"
        return 1
    end

    # Checked for every commit before the first one, so a file that changed
    # while the plan was on screen stops the run before anything lands.
    for file in $planned_files
        if string match -qr '(^/|(^|/)\.\.(/|$))' -- "$file"
            echo "smart-commit: unsafe path in commit plan: $file" >&2
            rm -f -- "$plan_file" "$log_file" "$message_file"
            return 1
        end

        if test -z "$(git -C "$repo_root" status --short -- "$file")"
            echo "smart-commit: planned file is no longer changed: $file" >&2
            rm -f -- "$plan_file" "$log_file" "$message_file"
            return 1
        end
    end

    for index in (seq 0 (math "$commit_count - 1"))
        set -l subject (jq -r --argjson index $index '.commits[$index].subject' "$plan_file")
        set -l files (jq -r --argjson index $index '.commits[$index].files[]' "$plan_file")

        # A path that is in neither the index nor the work tree has its
        # deletion staged already (a file removed with git rm, or the old name
        # after git mv). git add rejects it, but git commit --only still needs
        # it to record the deletion, so it is skipped here and kept below.
        # Ask Git rather than test -e, which on a case-insensitive file system
        # also finds the file under another case. One call covers the whole
        # commit; Git also lists the files under a matching directory, so
        # keep only exact names.
        set -l known_files (git -C "$repo_root" ls-files --cached --others --exclude-standard -z -- $files | string split0)
        set -l add_files
        for file in $files
            if contains -- $file $known_files
                set -a add_files $file
            end
        end

        if set -q add_files[1]
            git -C "$repo_root" add -A -- $add_files
            or begin
                set -l add_status $status
                rm -f -- "$plan_file" "$log_file" "$message_file"
                return $add_status
            end
        end

        # The message goes through a file because a fish list joins a
        # multi-line body with spaces when passed as one -m argument.
        jq -r --argjson index $index '.commits[$index] | .subject + (if .body == "" then "" else "\n\n" + .body end)' "$plan_file" >"$message_file"
        or begin
            set -l jq_status $status
            rm -f -- "$plan_file" "$log_file" "$message_file"
            return $jq_status
        end

        git -C "$repo_root" commit --only -F "$message_file" -- $files >"$log_file" 2>&1
        set -l commit_status $status
        if test "$commit_status" -ne 0
            cat "$log_file" >&2
            rm -f -- "$plan_file" "$log_file" "$message_file"
            return $commit_status
        end

        set_color $fish_color_quote
        printf '✓ '
        set_color normal
        printf '%s\n' "$subject"
    end

    rm -f -- "$plan_file" "$log_file" "$message_file"
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
