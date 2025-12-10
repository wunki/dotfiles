---
description: Smart commit with beads context and conventional commit format
---

Generate a commit message based on staged changes and beads context.

## Step 1: Gather Context

```bash
# What's staged?
git diff --cached --stat
git diff --cached --name-only

# What branch are we on?
git branch --show-current

# Recent commits for style reference
git log --oneline -5

# Current bead context (if any in-progress)
bd list --status in_progress --json 2>/dev/null | head -5
```

## Step 2: Analyze Changes

Read the staged diff to understand what changed:
```bash
git diff --cached
```

Categorize the change:
- `feat` - new feature
- `fix` - bug fix
- `refactor` - code restructuring without behavior change
- `docs` - documentation only
- `test` - adding/fixing tests
- `chore` - maintenance, deps, config
- `perf` - performance improvement
- `style` - formatting, whitespace

## Step 3: Identify Scope

From the changed files, determine scope:
- Single component/module → use that name
- Multiple related files → use parent directory/feature name
- Broad changes → omit scope

## Step 4: Check for Bead Reference

If there's an in-progress bead related to this work, include it:
```
feat(auth): add session refresh logic

Implements automatic token refresh before expiry.

Refs: bd-a1b2
```

## Step 5: Generate Commit Message

Format:
```
<type>(<scope>): <short description>

<body - what and why, not how>

[Refs: <bead-id>]
```

Rules:
- Subject line ≤ 72 chars
- Imperative mood ("add" not "added")
- No period at end of subject
- Body explains WHY, not just WHAT
- Reference beads if applicable

## Step 6: Present Options

Offer 2-3 commit message options:

```markdown
## Suggested Commits

### Option 1 (recommended)
```
feat(auth): add automatic token refresh

Refreshes JWT tokens 5 minutes before expiry to prevent
session interruption during long operations.

Refs: bd-a1b2
```

### Option 2 (minimal)
```
feat(auth): add token refresh
```

### Option 3 (detailed)
```
feat(auth): implement proactive JWT token refresh mechanism

- Add refresh check to auth middleware
- Create background refresh scheduler  
- Handle refresh failures gracefully

Refs: bd-a1b2
```

Which one? Or provide your own.
```

## Step 7: Execute (on confirmation)

Once user picks or provides message:
```bash
git commit -m "<message>"
```

If user says "1" or "option 1", use that option directly.
