---
name: ask-questions-if-underspecified
description: Pauses to ask focused clarifying questions when a request lacks enough
  information to execute correctly. Use when the task is vague, has multiple plausible
  interpretations, is missing acceptance criteria, or when wrong assumptions would
  produce significant wasted work. Don't use when the request is already well-specified,
  when requirements were clarified earlier in the same conversation, when the task
  is trivially unambiguous (e.g., fix a specific typo), or when general clarification
  techniques are being discussed rather than applied to a specific task.
---

# Ask Questions If Underspecified

## Goal

Ask the minimum set of clarifying questions needed to avoid wrong work. Never start
implementing until must-have questions are answered — or the user explicitly approves
proceeding with stated assumptions.

## Workflow

### 1. Check conversation history first

Before asking anything, scan earlier messages in the current thread. Requirements
already discussed are not underspecified. If prior messages resolve the ambiguity,
proceed directly to Step 4.

### 2. Decide whether the request is underspecified

After inspecting the codebase or task context as needed, treat a request as
underspecified if **two or more** of the following are unclear:

- Objective: what should change vs. stay the same
- Done criteria: acceptance criteria, examples, or edge cases
- Scope: which files, components, or users are in or out
- Constraints: compatibility, performance, style, deps, or time
- Environment: language/runtime versions, OS, build/test runner
- Safety: data migration, rollout/rollback, or rollback plan

If only one criterion is unclear and a low-risk discovery read can resolve it (e.g.,
checking a config file), do that read and proceed. Do not ask the user for something
that can be looked up.

If multiple plausible interpretations exist that would lead to meaningfully different
implementations, treat it as underspecified regardless of criterion count.

### 3. Ask must-have questions (one round only)

Ask 1–5 questions in a single pass. Prefer questions that eliminate entire branches
of work. Cap at one clarification round — if further ambiguity surfaces after answers
arrive, use judgment to pick the most reasonable interpretation rather than asking again.

Make questions easy to answer:

- Short, numbered questions; no paragraphs
- Offer multiple-choice options when possible
- Mark reasonable defaults clearly (bold the recommended option, or add a
  "Recommended" label above a code block and tag defaults inside)
- Include a fast-path reply: `defaults` accepts all recommendations
- Separate "Need to know" from "Nice to know" when it reduces friction
- Allow compact replies such as `1b 2a 3c`; restate the chosen options in plain
  language to confirm before proceeding

### 4. Pause before acting

Until must-have answers arrive:

- Never run commands, edit files, or produce a detailed plan that depends on unknowns.
- Do perform clearly labeled, low-risk discovery reads that do not commit to a
  direction (e.g., inspect repo structure, read relevant config files).

If the user explicitly says to proceed without answers (e.g., "just do it",
"make reasonable assumptions"):

1. State assumptions as a short numbered list.
2. Ask for a single confirmation ("Proceed with these? y/n").
3. Proceed only after they confirm or correct them.

### 5. Confirm interpretation, then proceed

Once answers are in, restate requirements in 1–3 sentences — including key constraints
and what success looks like — then start work.

## Question templates

```
Before I start, I need: (1) ..., (2) ..., (3) ....
If you don't care about (2), I will assume ....
```

```
Which of these should it be?
A) ... (default)
B) ...
C) Not sure — use default
```

```
What counts as "done"? For example: ...
```

```
Any constraints to follow (versions, performance, style, deps)?
If none, I will target the existing project defaults.
```

Compact multi-question format:

```
1) Scope?
   a) Minimal change (default)
   b) Refactor while touching the area
   c) Not sure — use default
2) Compatibility target?
   a) Current project defaults (default)
   b) Also support older versions: <specify>
   c) Not sure — use default

Reply: defaults | 1a 2b | correct any option
```

## Anti-patterns

- Never ask questions answerable by a low-risk discovery read (configs, existing
  patterns, docs).
- Never use open-ended questions when a tight multiple-choice or yes/no eliminates
  ambiguity faster.
- Never start a second clarification round after the user has already answered once.
- Never ask for confirmation on trivially unambiguous tasks (e.g., "fix the typo on
  line 42").
