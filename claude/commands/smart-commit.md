---
description: Analyze unstaged changes, group them into logical commits, and generate conventional commit messages
---

# Smart Commit - Atomic Commit Grouping

Analyze all unstaged changes in the working directory, intelligently group related changes into atomic commits, and generate conventional commit messages for each group.

## Step 1: Gather All Unstaged Changes

```bash
# Get list of modified files (unstaged)
git diff --name-only

# Get detailed diff of all unstaged changes
git diff

# Get status overview
git status --short
```

## Step 2: Analyze and Group Changes

For each changed file, analyze:
- **Purpose**: What is this change doing? (feature, fix, refactor, etc.)
- **Scope**: Which module/component/feature does it affect?
- **Relationship**: Which other changes are logically related?

Group changes into atomic commits based on:

### Grouping Rules

1. **Single Responsibility**: Each commit should do ONE logical thing
2. **Feature Coherence**: Changes to the same feature belong together
3. **Type Coherence**: Don't mix `feat` with `fix` in same commit unless tightly coupled
4. **File Relationships**:
   - Component + its tests = same commit
   - Component + its styles = same commit
   - Config changes = separate commit (usually `chore`)
   - Documentation = separate commit (usually `docs`)
5. **Dependency Order**: If commit B depends on commit A, A comes first

### Common Patterns

- `feat`: New functionality → group with related tests/types
- `fix`: Bug fix → group with regression test if added
- `refactor`: Code restructuring → can be larger, but still single purpose
- `chore`: Deps, config, tooling → usually standalone
- `docs`: Documentation only → standalone
- `style`: Formatting → standalone or skip if mixed with other changes

## Step 3: Present Commit Plan

Present the grouped commits in order of suggested execution:

```markdown
## Proposed Commits (in order)

### Commit 1: `feat(auth): add password reset flow`
Files:
- src/auth/reset-password.ts
- src/auth/reset-password.test.ts
- src/components/ResetPasswordForm.tsx

Summary: Implements the password reset feature with email verification.

---

### Commit 2: `fix(api): handle null response in user fetch`
Files:
- src/api/users.ts

Summary: Fixes crash when API returns null for deleted users.

---

### Commit 3: `chore: update eslint config`
Files:
- .eslintrc.js

Summary: Adds new linting rules for consistency.
```

## Step 4: Get User Confirmation

Ask the user:

1. Does this grouping make sense?
2. Should any commits be merged or split differently?
3. Ready to proceed with staging and committing?

## Step 5: Execute Commits (on confirmation)

For each commit group, in order:

```bash
# Stage only the files for this commit
git add <file1> <file2> ...

# Create the commit
git commit -m "<type>(<scope>): <description>"

# Repeat for next group
```

## Important Notes

- **Never auto-commit without confirmation**
- **Preserve the user's ability to modify grouping**
- If changes are too intertwined to separate cleanly, suggest committing together with a note
- If a file has multiple unrelated hunks, mention that `git add -p` could be used for finer control
- Always show the full plan before executing any commits
