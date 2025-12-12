---
description: Generate conventional commit message from staged changes
---

Generate a commit message based on staged changes.

## Step 1: Gather Context

```bash
# What's staged?
git diff --cached --stat
git diff --cached --name-only

# What branch are we on?
git branch --show-current

# Recent commits for style reference
git log --oneline -5
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

## Step 4: Generate Commit Message

Format:

```
<type>(<scope>): <description>

<body>
```

Rules:

- Subject line ≤ 72 chars
- Imperative mood ("add" not "added")
- No period at end of subject
- Body explains WHY, not just WHAT

## Step 5: Present Options

Offer 2-3 commit message options:

```markdown
## Suggested Commits

### Option 1 (recommended)
feat(auth): add automatic token refresh

Refreshes JWT tokens 5 minutes before expiry to prevent
session interruption during long operations.

### Option 2 (minimal)
feat(auth): add token refresh

### Option 3 (detailed)
feat(auth): implement proactive JWT token refresh mechanism

- Add refresh check to auth middleware
- Create background refresh scheduler
- Handle refresh failures gracefully

Which one? Or provide your own.
```

## Step 6: Execute (on confirmation)

Once user picks or provides message:

```bash
git commit -m "<message>"
```

If user says "1" or "option 1", use that option directly.
