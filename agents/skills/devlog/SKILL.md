---
name: devlog
description: Keep DEVLOG.md up to date and use it to resume project context in the user's personal developer-log style. Use when the user asks to update, write, continue, summarize, resume, catch up from, or maintain @DEVLOG.md / DEVLOG.md after or before a work session, or asks to capture what they did, learned, decided, got stuck on, where to continue next, or what happened last time.
---

# Devlog

Update the project's `DEVLOG.md` so it captures what the user did, learned, decided, and where to continue next. Also use `DEVLOG.md` to help the user jump back into a project by summarizing recent context and recommending the next concrete action.

This is not a generic changelog. It is a personal working journal with concrete shipped details and reflective notes. Treat it as both an end-of-session memory and a start-of-session handoff.

## Discovery

1. Find the target devlog:
   - Prefer an explicitly referenced `@DEVLOG.md` / path from the user.
   - Otherwise use `DEVLOG.md` in the current working directory.
   - If no obvious target exists, search upward from `cwd`; if still unclear, ask.
2. Read the existing `DEVLOG.md` before writing.
3. Gather evidence from the real work:
   - `git status --short`
   - `git diff --stat`
   - `git diff`
   - `git diff --cached`
   - recent commits if relevant: `git log --oneline --decorate -5`
   - the current conversation/session context
   - project docs or tests only when needed to understand the change
4. Do not invent facts. If an important detail is missing, ask the user instead of filling it in.

## Resume Workflow

Use this workflow when the user asks things like "where were we?", "catch me up", "resume this project", "what should I do next?", "read the devlog", or `devlog resume`.

1. Find and read the target `DEVLOG.md` using the discovery rules above.
2. Read the latest entry first. Include earlier entries only if they clarify context or the latest entry is too thin.
3. Optionally inspect current repo state when useful:
   - `git status --short`
   - `git diff --stat`
   - recent commits: `git log --oneline --decorate -5`
4. Summarize the current project state in the user's language:
   - what was last shipped or made real
   - what was learned
   - decisions that still matter
   - gotchas or blockers
   - the current mental model, if one is present
5. Extract the actionable continuation items, especially `NEXT`, `BLOCKED`, and `RESEARCH` bullets.
6. Recommend the first concrete next action. Prefer one small step that can be started immediately.
7. If repo state conflicts with the devlog, call that out plainly before recommending next steps.

Do not rewrite `DEVLOG.md` during a resume unless the user also asks to update it.

### Resume Response Shape

Keep the response short and useful:

```markdown
Last time:
- ...

Where things stand:
- ...

Next move:
1. ...

Watch out for:
- ...
```

Use bullets, not a long essay. Include exact file paths, commands, endpoints, or symbols when they help the user restart quickly.

## Writing Style Profile

Match this style:

- First-person, reflective, and direct.
- Casual but precise: "Wrote some Go again today", "That was a long time ago", "Still a delightful and easy language to use."
- Explain the work as a short story before listing facts.
- Prefer concrete nouns over abstract summaries: route names, hostnames, commands, endpoint paths, deployment names, status responses.
- Capture the mental model when useful: "The user's browser talks to ... The user's CLI talks to ..."
- Name the challenge and the lesson in plain language.
- Use short paragraphs. A paragraph can be one or two sentences if that is enough.
- Keep the tone warm, pragmatic, curious, and slightly informal.
- Avoid corporate release-note language, inflated claims, or generic productivity summaries.
- Avoid over-polishing. It should still feel like a human note written at the end of a work session.

Good style examples:

```markdown
Wrote some Go again today. That was a long time ago. Still a delightful and easy language to use.
```

```markdown
As a mental model. The user's browser talks to `*.tonen.site`. The user's CLI talks to `tunnel.tonen.app`.
```

```markdown
Tomorrow I should continue by decoding and validating that JSON, then actually registering a tunnel and returning a preview URL.
```

## Entry Shape

Keep the existing top-level heading:

```markdown
# Developer Log
```

Entries use this date heading format:

```markdown
## Saturday, Jun 27, 2026
```

For a new day, insert the new entry directly under `# Developer Log`, above older entries.

For an existing day, update that day's entry instead of creating a duplicate.

Each entry should usually contain:

1. 2-5 narrative paragraphs describing the work, challenge, mental model, and why it matters.
2. A concise bullet ledger using uppercase labels.

Use whichever labels fit the actual work:

- `SHIPPED`: concrete things completed or now working
- `FIXED`: bugs resolved
- `CHANGED`: notable implementation changes that are not standalone shipped items
- `DECISION`: product, architecture, or workflow decisions
- `LEARNED`: concepts, APIs, tools, docs, or operational lessons
- `GOTCHA`: surprising problems, sharp edges, or failure modes
- `BLOCKED`: current blockers and what is needed to unblock
- `NEXT`: immediate continuation points, written as actionable steps
- `RESEARCH`: questions to investigate
- `SOMEDAY`: useful ideas that should not distract from the current path

Do not force every label. Prefer 5-12 strong bullets over a long exhaustive list.

## What To Capture

Prioritize:

- What actually changed or shipped.
- What the user learned and how they learned it.
- Decisions and why they were made.
- Gotchas that future-you would otherwise rediscover.
- Exact next steps to restart quickly tomorrow.
- Links to useful docs, issues, PRs, posts, or commands when they were part of the work.

Use inline code for:

- commands
- paths
- endpoints
- hostnames
- package names
- symbols/types/functions
- status strings

When there is a useful continuation point, write it in the user's style:

```markdown
Tomorrow I should continue by ...
```

Then mirror it in one or more `NEXT` bullets.

## Safety and Accuracy

- Never add secrets, tokens, private credentials, `.env` values, or sensitive personal data.
- Do not claim something shipped unless there is evidence from the repo, commands, or the user.
- If tests were not run, say so only when it matters. Do not fabricate verification.
- If the work is uncertain, write uncertainty plainly: "I think the next step is ..." only if that matches the evidence.
- Preserve old entries unless intentionally updating today's entry.
- Keep links and code spans valid Markdown.

## Editing Workflow

1. Read the whole target `DEVLOG.md`.
2. Determine today's date using the actual current date.
3. Build a compact evidence list from git/session state.
4. Draft the entry in the user's style.
5. Edit `DEVLOG.md`:
   - insert a new dated section under the title, or
   - replace today's existing section with an updated version.
6. Re-read the edited area to verify formatting and that no old content was accidentally removed.
7. In the final response, summarize:
   - changed file
   - whether a new entry was added or today's entry was updated
   - any facts you could not verify

## If There Is Not Enough Context

Ask focused questions instead of guessing. Useful questions:

- What did you actually get working today?
- What surprised you or took longer than expected?
- What should tomorrow-you do first?
- Were any decisions made that we should preserve?
- Are there links, commands, endpoints, or error messages worth saving?

Prefer asking 2-4 questions at once rather than a long interview.
