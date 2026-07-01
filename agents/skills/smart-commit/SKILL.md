---
name: smart-commit
description: Analyzes all uncommitted changes (staged and unstaged), groups them into atomic commits by logical purpose, and generates conventional commit messages with user-facing bodies. Use when the user asks to "smart commit", "group my commits", "split into atomic commits", "organize my changes into commits", or when there are many uncommitted changes across multiple files that should be separated into well-scoped commits. Don't use for a single, simple commit message request, for push/pull operations, for rebasing or squashing existing commits, or when the user just says "commit this" without asking for grouping or splitting.
---

# Smart Commit — Atomic Commit Grouping

Analyze all uncommitted changes in the working directory, intelligently group related changes into atomic commits, and generate conventional commit messages for each group.

## Step 1: Gather All Uncommitted Changes

```bash
# Get full status including staged, unstaged, and untracked files
git status --short

# Get diff of unstaged tracked changes
git diff

# Get diff of staged changes
git diff --cached

# Get list of untracked files
git ls-files --others --exclude-standard
```

Note: all four commands are needed to get a complete picture. Untracked files appear only in `git ls-files --others`. Staged changes appear only in `git diff --cached`.

## Step 2: Analyze and Group Changes

For each changed file, analyze:
- **Purpose**: What is this change doing? (feature, fix, refactor, etc.)
- **Scope**: Which module, component, or feature does it affect?
- **Relationship**: Which other changes are logically related?

Group changes into atomic commits using these rules:

### Grouping Rules

1. **Single Responsibility**: Each commit does ONE logical thing.
2. **Feature Coherence**: Changes to the same feature belong together.
3. **Type Coherence**: Do not mix `feat` with `fix` in the same commit unless tightly coupled.
4. **File Relationships**:
   - Component + its tests = same commit
   - Component + its styles = same commit
   - Config changes = separate commit (usually `chore`)
   - Documentation = separate commit (usually `docs`)
5. **Dependency Order**: If commit B depends on commit A, A comes first.

### Type Reference

- `feat`: New functionality — group with related tests and types.
- `fix`: Bug fix — group with regression test if added.
- `refactor`: Code restructuring — can be larger, but still single purpose.
- `chore`: Deps, config, tooling — usually standalone.
- `docs`: Documentation only — standalone.
- `style`: Formatting — standalone, or omit if entangled with other changes.

## Step 3: Present Commit Plan

Present the grouped commits in suggested execution order:

```
## Proposed Commits (in order)

### Commit 1: `feat(auth): add password reset flow`
Files:
- src/auth/reset-password.ts
- src/auth/reset-password.test.ts
- src/components/ResetPasswordForm.tsx

Body: Users can now reset their password via email if they forget it.

---

### Commit 2: `fix(api): handle null response in user fetch`
Files:
- src/api/users.ts

Body: Fixes a crash that occurred when viewing a deleted user's profile.

---

### Commit 3: `chore: update eslint config`
Files:
- .eslintrc.js

Body: (none)
```

### Commit Body Guidelines

The body appears in release notes and should be readable by non-technical users. Write one sentence describing user impact, not implementation.

Skip the body entirely for `chore`, `style`, and `ci` commits.

Good: "Users can now export their data as a CSV file."
Good: "Fixes an issue where the app would freeze when uploading large images."

Bad: "Added exportToCSV function to DataService." (implementation detail)
Bad: "Fixed null pointer exception in handleUpload." (too technical)

## Step 4: Get User Confirmation

Ask the user how to proceed interactively when a question tool is available. Prefer Pi's `question` tool for the confirmation prompt:

```
question: "How would you like to proceed with these N commits?"
options:
  - label: "Proceed"
    description: "Commit all groups as planned"
  - label: "Skip some"
    description: "Choose which commits to skip"
  - label: "Edit grouping"
    description: "Adjust how changes are grouped"
  - label: "Custom"
    description: "Type specific instructions"
```

If Pi's `question` tool is not available in the active tool list, fall back to numbered prose with the same options.

Handle responses:
- **Proceed**: Execute all commits as planned.
- **Skip some**: Ask which commit numbers to skip, then execute the rest.
- **Edit grouping**: Ask what to change, revise the plan, present it again, confirm once more before executing.
- **Custom**: Ask for the instruction, apply it to the plan, show the updated plan, confirm again.

Do not execute any commits until the user has explicitly confirmed.

## Step 5: Execute Commits

For each commit group in order:

```bash
# Stage only the files for this commit
git add <file1> <file2> ...

# For deleted files, use:
git rm <deleted-file>

# For new untracked files, use:
git add <new-file>

# Commit with subject line only (chore/style/ci):
git commit -m "<type>(<scope>): <description>"

# Commit with subject and body (feat/fix/refactor/docs):
git commit -m "<type>(<scope>): <description>" -m "<body>"
```

Omit the second `-m "<body>"` argument entirely for `chore`, `style`, and `ci` commits.

After all commits are done, confirm the result:

```bash
git log --oneline -<N>
```

Show the user the resulting commit log.

## Error Handling

- **No uncommitted changes**: Report "No uncommitted changes found." and stop.
- **Changes too intertwined to separate cleanly**: Suggest committing together with a note explaining why splitting would be misleading. Offer `git add -p` as an option for finer-grained hunk control if the user wants to try.
- **Merge conflict markers present**: Stop, report the files with conflicts, and tell the user to resolve conflicts before running smart-commit.
- **git command fails**: Show the raw error output and stop. Do not retry or work around silently.
- **Scope is unclear**: Use the directory name or top-level module as the scope. Note the assumption in the plan.
