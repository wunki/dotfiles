# PR Description

A Pi-compatible skill for drafting consistent, reviewer-centered PR descriptions in a context-first narrative style.

## When to use

Use when asked to write, rewrite, or update a pull request description/body, especially when the user wants a consistent, context-rich PR narrative.

## What it covers

- Gather PR context from `gh` and `git`.
- Explain the context/problem before implementation details.
- Group changes by concept rather than by file.
- Use clear `Context`, `What changed`, `Why`, and `Notes` sections.
- Include before/after examples for changed shapes, layouts, APIs, or behavior.
- Leave routine test/build status to CI/CD.
- Surface migration, compatibility, rollback, manual-check, and reviewer-focus notes.

## Usage

```text
/pr-description
```

Example requests:

```text
Write a PR description for the current branch.
Rewrite this PR body in the preferred style.
Update https://github.com/org/repo/pull/123 with a better description.
Draft a context-first PR description from my diff.
```
