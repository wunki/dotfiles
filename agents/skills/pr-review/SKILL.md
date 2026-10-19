---
name: pr-review
description: >
  Reviews a pull request or diff for correctness first, then for how easily a human can
  understand, trust and change it: clear abstractions, data-driven design, honest names,
  local reasoning, and tests at the right boundary. Produces verified, ranked findings with
  concrete fixes. Use when asked to review a PR, review a diff or branch before merging,
  do a code review, check whether code is ready to merge, or act as the reviewer bot. Don't
  use for writing the change itself, auditing a whole codebase without a diff, or pruning
  tests (use test-audit).
---

# PR Review

The question behind every review: **will the next engineer, six months from now, understand this code, trust it, and change it safely?** Correctness comes first. A correct change that only its author can follow is still a cost the team pays forever, so readability, abstractions and data design are real findings, not style notes.

Read [LENSES.md](LENSES.md) before reviewing anything non-trivial. It holds the checklists and the smell-to-fix examples for each lens.

## 1. Orient

- Read the PR description, linked tickets and any design notes. Know what "done" was supposed to mean.
- Read the repo's rules: root and scoped `AGENTS.md` / `CLAUDE.md`, architecture and glossary docs, and contributing docs. Repo rules beat this skill when they conflict.
- Find how CI tests the code (`.github/workflows`, package scripts, Makefile). CI is the source of truth for "does it pass".
- Get the change: `gh pr view <n>`, `gh pr diff <n>`, or `git diff <base>...<head>`. Check the head out in a separate worktree, so you read whole files and can run things without touching anyone's checkout.

Never review hunks alone. For every changed file, read the whole file. For every changed function, read its callers and the code it calls. Most real bugs live in the lines the diff didn't show.

## 2. Map the change

Before judging anything, write down in three to six lines:

- the concepts this PR introduces, moves, renames or deletes;
- where data enters, how it is transformed, where it is stored, and where it leaves;
- which invariants or contracts (wire formats, storage, IPC, public interfaces) it touches.

For a non-trivial flow, sketch it as a small ASCII diagram. If you can't summarize the change simply, that is itself a finding: the design or the PR's scope is too tangled.

## 3. Review through the lenses, in order

1. **Correctness and failure paths.** Edge cases, async ordering and races, stale results after a switch, time and calendar math, unknown input, error handling, persisted-data compatibility, security and data egress.
2. **Data-driven design.** Is variation expressed as data (a table, map, registry or schema) or as branching scattered across files? Do the types make invalid states unrepresentable? Is there one source of truth, with derived values derived instead of stored twice?
3. **Abstractions.** Use the `codebase-design` vocabulary (module, interface, depth, seam, leverage, locality). Apply the deletion test. Flag pass-through wrappers, premature seams, leaky interfaces and logic living in the wrong owner.
4. **Human readability.** Names from the domain glossary that say what, not how. Straight control flow. A function you can understand without opening five files. Comments that explain why. Consistency with the surrounding code.
5. **Tests.** Behavior asserted at the strongest boundary, regressions that fail without the fix, no tests of implementation details, no missing case for a real risk.
6. **Change hygiene.** Scope creep, dead code left behind, outdated docs, breadcrumbs, unrelated changes.

For a large PR (roughly 40+ files or several owner areas), split the review by owner area or by lens and run reviewers in parallel (subagents, if available). Give each one the PR context, the repo rules and the lens list. Then verify and dedupe their findings yourself (step 4). A reviewer's claim is a lead, not a finding.

## 4. Verify every finding

A finding survives only if you can show it:

- **Bug claims:** trace the exact path with real values, and reproduce it with a failing test or a precise scenario when practical ("account A's scan resolves after switching to B, so `x` is written to B's store"). If you can't make it concrete, drop it or downgrade it to a question.
- **Design claims:** name the concrete cost: the second place someone must edit to add a new kind, the caller that has to know an internal detail, the state that can disagree with its source. "Could be cleaner" is not a finding.
- **Readability claims:** point at the exact line a reader stumbles on and show the clearer version.

Don't report what a formatter, linter or type checker already enforces. Don't restate the diff back to the author. Don't invent problems to fill a section.

## 5. Rank

- **Blocker:** wrong behavior, data loss or corruption, a security or privacy leak, a broken contract or migration, a test that passes for the wrong reason.
- **Should fix:** a design or readability problem with a concrete maintenance cost, or a missing test for a real risk.
- **Consider:** a better alternative that's worth weighing but optional.
- **Question:** you can't tell the intent. Ask instead of asserting.

## 6. Report

```
Verdict: Approve | Approve with follow-ups | Request changes

<2–3 sentences: what the PR does, how it's built, and the single biggest risk.>

Blockers / Should fix / Consider / Questions
- <file:line> <one-line claim>
  Why: <the concrete failure or cost>
  Fix: <the specific change, with a short snippet when it helps>

What I checked: <areas read in full, tests or scenarios run>
What I didn't: <what was out of reach, e.g. not run against real data>
```

Keep it direct and specific. One sentence on what's genuinely well done is fine when it's notable and useful to know. Skip praise padding and filler.

## Posting

Report in the conversation by default. Post to the PR only when the user asks:

- Use one review with inline comments on the exact lines (`gh api repos/{owner}/{repo}/pulls/{n}/reviews`), or one summary comment. Don't spray separate comments.
- Never approve or request changes on someone's behalf unless explicitly asked. If the repo has a review-gate or bot convention (a label, a marker comment), follow its docs exactly, and say plainly what your review does and doesn't satisfy (an agent review usually can't replace a required human approval).
