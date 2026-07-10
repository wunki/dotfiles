---
name: pr-description
description: >
  Draft or update clear, reviewer-centered pull request descriptions. Use when the
  user asks for a PR description/body, GitHub PR body, PR summary, context-first
  PR narrative, to update an existing PR description, or to turn commits/diffs
  into a consistent PR narrative. Do not use for commit messages unless the user is
  explicitly writing a PR description.
---

# PR Description — Context-First Reviewer Narrative

Write PR descriptions that are precise, reviewer-friendly, and narrative: context first, grouped changes second, rationale and notes last. The goal is to make a reviewer understand *why this exists*, *what changed*, *where to focus*, and *what risks or edges matter* without reading every file first.

Good PR descriptions have a clear thesis, enough background to make the diff obvious, concrete examples where shape or behavior changed, explicit tradeoffs, and direct risk/edge callouts. They respect the reviewer's time by separating narrative from mechanics and leaving routine test status to CI/CD.

This style is calibrated to the user's preferred PR shape. Open directly with the context paragraph—do not add a `## Context` heading:

```markdown
<Context paragraph explaining why the PR exists.>

## What changed

### Focused subsystem / behavior

## Why

## Notes
```

Use this structure as a default, but shrink or expand it to fit the PR.

## Core Style Principles

1. **Start directly with context, not a heading or implementation.** The opening paragraph should explain the situation, constraint, or bug that made the change necessary. Never prefix it with a `Context` heading.
2. **Tell a story.** Move from problem → approach → important details → reason/tradeoffs → proof.
3. **Group by concept, not file.** Use `###` subsections for meaningful units of change, such as migration, storage model, API behavior, UI state, tests, or rollout.
4. **Be concrete.** Include names of important commands, paths, config keys, routes, state keys, data shapes, or user-visible behavior when they help review.
5. **Show before/after when it clarifies.** Use small code/text blocks for schema, filesystem layout, API shape, CLI output, or behavior changes.
6. **Make ownership and boundaries explicit.** Call out what is included, excluded, global vs scoped, backend vs frontend, migration vs steady state, etc.
7. **Let CI/CD own routine validation.** Do not include a default validation/testing section. Mention manual verification only when it is review-relevant and not covered by CI.
8. **Surface risk and reviewer focus.** Put rollback, compatibility, migration, known gaps, manual checks, and contract changes in `## Notes` or `## Risks`.
9. **Avoid generic checklist filler.** Prefer compact prose and purposeful bullets over templates like "Summary / Testing" when more context is needed.
10. **Keep it reviewable.** Long PRs can be detailed; small PRs should be concise.

## Discovery Workflow

Before drafting, gather enough evidence to avoid vague or invented content.

### Existing GitHub PR

If the user provides a PR URL or asks to update the current PR:

```bash
gh pr view <url-or-branch> --json title,body,baseRefName,headRefName,author,commits,files,closingIssuesReferences

gh pr diff <url-or-branch>
```

Also inspect relevant files if the diff alone does not explain intent.

### Current branch without a PR URL

```bash
git status --short
git branch --show-current
git remote -v
git log --oneline --decorate --max-count=20
```

Find the likely base branch, then inspect commits and diff:

```bash
git merge-base HEAD origin/main || git merge-base HEAD main
git log --oneline <base>..HEAD
git diff --stat <base>..HEAD
git diff <base>..HEAD
```

If the repo uses `master`, `develop`, or a release branch, choose that base instead and state the assumption.

### Working tree changes

If there are uncommitted changes, ask whether to include them unless the user clearly asked for a draft from the working tree. Do not silently mix unrelated local edits into a PR description.

### Issue/task context

Look for issue IDs in the branch name, PR title, commits, or diff. If available, inspect them:

```bash
gh issue view <id-or-url>
gh pr view <related-pr-or-url>
```

Use issue context to explain the problem and acceptance criteria, but do not claim the PR closes an issue unless the evidence supports it.

## Information to Extract

Build a brief internal outline before writing:

- **Problem/context:** What existed before? What was unsafe, missing, confusing, slow, broken, or hard to extend?
- **User/system impact:** Who benefits and what changes for them?
- **Main changes:** 2–5 conceptual groups, not a file-by-file list.
- **Important mechanics:** Migration behavior, data ownership, compatibility, contracts, auth/security/privacy implications, background jobs, failure modes.
- **Exclusions:** What intentionally did not change?
- **Manual/review-only evidence:** Screenshots, recordings, manual flows, fixture updates, or smoke checks only if they matter beyond CI/CD.
- **Risks/notes:** Rollback, migration, performance, edge cases, future follow-ups, reviewer focus.

If any of these are important but unknown, ask a concise question before finalizing. If the user wants a quick draft, include `TODO:` placeholders rather than inventing details.

## Default Template

Use this for medium or large PRs:

```markdown
<1–3 opening paragraphs explaining the prior state, problem, constraint, or goal.>

## What changed

### <Conceptual change 1>

<What changed and how reviewers should understand it. Include before/after blocks if helpful.>

### <Conceptual change 2>

<Another grouped change.>

### <Conceptual change 3>

<Another grouped change, if needed.>

## Why

- <Reason this approach solves the problem or reduces risk.>
- <Tradeoff, safety property, product value, or maintenance benefit.>
- <Compatibility or rollout rationale.>

## Notes

- <No contract changes / migration behavior / rollback note / known limitation / reviewer focus.>
- <Optional: manual verification that is not covered by CI/CD, if review-relevant.>
```

Do not include empty sections. Rename `## Notes` to `## Risks`, `## Rollout`, or `## Compatibility` when that is more precise.

## Size-Based Variants

### Small PR

For narrow bug fixes, docs changes, or small refactors:

```markdown
<Opening paragraph explaining what was wrong or why this small change exists.>

## What changed

<1 short paragraph or 2–4 bullets.>
```

Optionally add `## Notes` only if there is a non-obvious caveat or manual check not covered by CI/CD.

### Large, Risky, or Migration PR

For auth, privacy, persistence, migrations, schemas, public APIs, billing, security, or cross-service contracts, include more explicit sections:

```markdown
<Opening context paragraphs explaining why the PR exists.>

## What changed

### <Data/model/storage/API boundary>

### <Migration or compatibility behavior>

### <Call-site or user-facing behavior>

## Scope

<What is included and what remains intentionally unchanged.>

## Why

## Notes / Rollback
```

Prefer detailed, explicit statements over brevity for these PRs.

## Before/After Blocks

Use before/after examples when the change affects shape or behavior:

````markdown
Before:

```text
<old shape>
```

After:

```text
<new shape>
```
````

Keep examples small and accurate. Do not paste huge diffs.

## CI/CD and Manual Checks

Do not include a default `## Validation` or `## Testing` section. CI/CD is expected to show routine test, lint, typecheck, and build status.

Only mention verification when it adds information CI/CD cannot provide, such as:

- Manual product flow checks.
- Screenshots or recordings for UI behavior.
- Migration/rollback smoke checks.
- External integration checks.
- Explicitly skipped or impossible checks that reviewers should know about.

Put those notes under `## Notes`, `## Risks`, or `## Rollout` rather than creating a separate validation section.

## Tone and Wording

Prefer:

- "This makes account ownership visible at call sites."
- "The migration is idempotent and never overwrites existing scoped data."
- "No backend route contract changes."
- "Existing installs migrate on next sign-in when the destination account is known."

Avoid:

- "This PR refactors stuff."
- "Various fixes."
- "Tests pass" or routine CI status that GitHub already shows.
- Marketing language or unnecessary superlatives.
- A file-by-file changelog unless the PR is a generated/vendor update.

## Output Behavior

- By default, return only the proposed PR description in a fenced `markdown` block.
- If the user asks for a title too, provide a concise conventional title before the body.
- If updating GitHub, first show the body unless the user explicitly asked to apply it directly. Then use a temporary file and `gh pr edit --body-file <file>` to avoid shell quoting issues.
- Never merge, close, retarget, or otherwise mutate PR state unless the user explicitly asks and confirms.

## Final Self-Check

Before handing off, verify the draft:

- Does the description open with an unheaded paragraph that explains why the PR exists?
- Are changes grouped by reviewer-relevant concepts?
- Are important boundaries and exclusions explicit?
- Are manual checks omitted unless they add information beyond CI/CD?
- Are risks, migrations, contracts, or rollback notes called out if relevant?
- Is the description concise enough for the PR size?
