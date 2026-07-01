# Smart Commit

A Claude Code skill that analyzes unstaged changes, groups them into logical atomic commits, and generates conventional commit messages.

## When to use

When asked to "smart commit", "group commits", "atomic commits", or when there are many unstaged changes that should be split into separate, well-scoped commits.

## What it covers

- **Step 1**: Gather all unstaged changes
- **Step 2**: Analyze and group by purpose, scope, and relationship
- **Step 3**: Present commit plan with conventional commit messages
- **Step 4**: Get user confirmation before executing
- **Step 5**: Execute commits in dependency order

## Grouping rules

- Single responsibility per commit
- Feature coherence (component + tests together)
- Type coherence (don't mix `feat` with `fix`)
- Dependency ordering (if B depends on A, commit A first)

## Usage

```
/smart-commit
```
