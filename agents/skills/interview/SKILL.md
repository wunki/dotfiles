---
name: interview
description: Conducts exhaustive requirements-gathering interviews for software features or systems. Reads existing context, then asks structured numbered questions covering purpose, technical design, UI/UX, edge cases, security, and rollout. Use when the user says "interview me", "deep dive on requirements", "spec this out", "fill out the spec", "help me think through this feature", or when a feature needs thorough requirements elicited before a spec or plan can be written. Don't use for quick clarifying questions during an in-progress task (use ask-questions-if-underspecified instead), for writing or updating a SPEC.md document (use create-spec instead), for generating a task plan (use create-plan instead), or for general architectural discussions that don't need structured requirements output.
---

# Interview

Exhaustive requirements elicitation through deep, structured questioning. This skill gathers requirements; it does not write specs or plans.

## When to Use

- User explicitly asks to be interviewed about a feature
- Starting a complex feature that needs thorough thinking before any code is written
- Existing SPEC.md has gaps that need filling
- Any task where requirements must be extracted before action

## Workflow

### 1) Read Existing Context

Before asking anything:

1. Check if `SPEC.md` exists in the project root (cwd). If yes, read it.
2. Check for `README.md`, any files in `docs/`, and any existing code files directly related to the feature being discussed.
3. Note what is already documented vs. what is missing or ambiguous.

Do not ask questions you can already answer from existing context.

If no existing context is found (greenfield project), skip directly to Step 2 and open with purpose and scope questions.

### 2) Interview Deeply

Cover all areas below. Ask only non-obvious questions — things that cannot be discovered by reading the codebase.

| Area | What to explore |
|------|-----------------|
| **Purpose** | What problem? Why now? What triggered this? What does success look like? |
| **Technical** | Architecture choices, data model, APIs, integrations, performance requirements |
| **UI/UX** | User flows, states, feedback, accessibility, responsive behavior |
| **Edge cases** | What could go wrong? Invalid inputs, race conditions, failure modes |
| **Tradeoffs** | What are we optimizing for? What are we willing to sacrifice? |
| **Security/Privacy** | Data sensitivity, auth requirements, audit needs |
| **Testing** | How do we verify it works? What is critical to test? |
| **Rollout** | Feature flags? Migration? Backwards compatibility? |

**Prioritization:** Always lead with Purpose questions. Then Technical. Adjust order based on what existing context already covers — skip areas the codebase has answered. Security/Privacy and Edge cases are never skippable; surface them even if the user seems impatient.

**Question format:**
- Number each question; use lettered options when choices exist
- Suggest a reasonable default and mark it clearly (e.g., "recommended")
- Include "Not sure — use default" as the last option on choice questions
- Allow compact responses like `1a 2b 3c`
- Keep each question to one or two lines — no paragraphs

**Pacing:**
- Start with 3–5 questions covering Purpose and the biggest Technical unknowns
- After each response, acknowledge what was received, then ask targeted follow-ups
- If a user skips a numbered question in their reply, treat it as "Not sure — use default" and note the assumed default before moving on
- Continue until all eight areas have sufficient coverage

**Terse or "I don't know" answers:** If the user gives a non-answer on a critical area (security, edge cases, rollout), note the gap explicitly, propose a safe default, confirm the user accepts it, and move on. Do not loop on the same question.

**Challenge the premise:** If the approach described seems wrong, say so early and explain why. Redirect before requirements harden.

### 3) Determine Completion

The interview is complete when BOTH of the following are true:
- Each of the eight areas has either an answered question or an agreed-upon default or accepted risk
- The user confirms nothing critical is missing (Step 4)

Do not continue interviewing indefinitely. When coverage is sufficient across all areas, proceed to Step 4.

### 4) Confirm Understanding

1. Write a brief summary (bullet points) of what was learned in each area.
2. Ask: "Does this capture everything, or is there anything missing or wrong?"
3. Iterate on the summary if the user corrects anything.

### 5) Hand Off

After confirmation, determine the next action based on what the user has asked for. If not stated, ask via `AskUserQuestion` (single-select):

- "Write the spec" — invoke `create-spec` skill
- "Build the plan" — invoke `create-plan` skill
- "Jump to implementation" — proceed directly

- User asked for a spec, or SPEC.md exists and needs updating → invoke the `create-spec` skill
- User asked for a task plan or implementation steps → invoke the `create-plan` skill
- User asked to proceed directly with implementation → summarize the agreed requirements as a short context block, then proceed
- User ends the conversation without a clear next step → output the requirements summary and note which skill to invoke when ready

## Anti-patterns

- **Don't ask obvious questions**: If the codebase already answers it, don't ask
- **Don't front-load everything**: Start broad, drill down based on answers
- **Don't skip security or edge cases**: Even if the user seems impatient, surface them — note defaults if needed
- **Don't interview forever**: Eight areas with coverage is done; summarize and hand off

## Example Interview Flow

```text
I've read the existing SPEC.md and codebase. A few questions before I fill in the gaps:

1) User authentication - which approach?
   a) Email/password only (simplest)
   b) OAuth providers (Google, GitHub)
   c) Both (recommended for flexibility)
   d) Not sure - recommend what fits

2) Session handling?
   a) JWT in HTTP-only cookie (recommended for web)
   b) JWT in localStorage
   c) Server-side sessions with Redis
   d) Not sure - use recommended

3) What happens on auth failure?
   a) Redirect to login page
   b) Show inline error, stay on page
   c) Depends on context - describe: ___

Reply with: 1c 2a 3a (or describe)
```

After receiving answers, dig deeper:

```text
Got it: OAuth + email/password, JWT cookies, redirect on failure.

Follow-up on OAuth:

4) Which providers are must-have vs nice-to-have?
   a) Google only (most users have it)
   b) Google + GitHub (good for dev tools)
   c) Let users choose from: ___

5) Account linking - if user signs up with email, then tries Google with same email?
   a) Link accounts automatically
   b) Prompt user to link or keep separate
   c) Reject - must use original method
```

Continue until requirements are complete, then use `create-spec` to write the spec.
