---
model: claude-sonnet-4-5-20250929
description: Remove a git worktree and clean up associated resources
argument-hint: [branch-name]
allowed-tools: Bash, Read, Write, Edit, Glob, Grep
---

# Git Worktree Removal for Elixir

Safely remove a git worktree and clean up all associated resources including the branch and database.

## Arguments

- `$ARGUMENTS` - Branch name of the worktree to remove (required)

## Instructions

### 1. Validate Arguments

Ensure a branch name is provided in `$ARGUMENTS`. If not provided, ask the user for a branch name.

### 2. Generate Worktree Slug

Convert the branch name to the directory name by replacing `/` with `-`:

```
WORKTREE_SLUG = $ARGUMENTS with "/" replaced by "-"
```

### 3. Verify Worktree Exists

Check that the worktree exists:

```bash
git worktree list | grep "trees/$WORKTREE_SLUG"
```

If not found, inform the user and stop.

### 4. Identify Port

Read the port from the worktree's `.env` file:

```bash
grep "^PORT=" trees/$WORKTREE_SLUG/.env
```

### 5. Stop Running Services

If a port was found, kill any processes using it:

```bash
lsof -ti :$PORT | xargs kill -9 2>/dev/null || true
```

### 6. Remove the Worktree

```bash
git worktree remove trees/$WORKTREE_SLUG
```

If this fails (uncommitted changes, etc.), ask the user if they want to force removal:

```bash
git worktree remove --force trees/$WORKTREE_SLUG
```

### 7. Clean Up Orphaned Files

Check for and remove any lingering files or lock artifacts:

```bash
# Remove any leftover directory
rm -rf trees/$WORKTREE_SLUG 2>/dev/null || true

# Prune worktree references
git worktree prune
```

### 8. Delete the Branch

Delete the local branch:

```bash
git branch -d $ARGUMENTS
```

If this fails (unmerged changes), ask the user if they want to force delete:

```bash
git branch -D $ARGUMENTS
```

### 9. Drop the Database

Drop the PostgreSQL database associated with this worktree:

```bash
mix ecto.drop --repo AppName.Repo
```

Or directly via psql:

```bash
dropdb app_name_dev_$WORKTREE_SLUG
```

Note: Determine the actual app name from `mix.exs` before running.

### 10. Report

After successful removal, report:

```
Worktree removed successfully!

Removed:
- Worktree: trees/$WORKTREE_SLUG
- Branch: $ARGUMENTS
- Database: app_name_dev_$WORKTREE_SLUG

Remaining worktrees:
  [output of git worktree list]
```

## Warnings

- Worktree and branch deletion are permanent and cannot be undone
- Uncommitted changes will be lost if force removal is used
- Database deletion is irreversible
