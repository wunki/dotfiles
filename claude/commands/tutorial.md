---
description: Generate an exhaustive implementation tutorial instead of code
---

Think with maximum depth and thoroughness. Use extended thinking to its fullest capacity — take as long as needed.

## Task

Create a comprehensive implementation tutorial for a developer to follow. Do NOT implement the change yourself or create a PR — only write the tutorial document.

**Change requested:** $ARGUMENTS

## Goal

The reader should emerge as a **deep expert** in the concepts covered—not just someone who followed steps. They should understand:
- Why things work the way they do
- How the internals function
- When to use (and not use) these patterns
- How to debug when things go wrong
- How to apply these concepts in new situations

This is not a "follow along and copy" tutorial. It's a deep learning experience.

**Audience:** An intermediate Elixir developer who knows the language well, but wants to deepen their understanding of Phoenix, LiveView, and the BEAM.

## Research Phase (Do This First)

Before writing anything, thoroughly explore the codebase:
- Use Glob and Grep to find all relevant files
- Read the files that will be affected by this change
- Trace data flow and dependencies
- Understand existing patterns and conventions used in this project
- Look at similar past implementations for reference

Do not start writing until you deeply understand the system.

## Tutorial Philosophy

**Problem-first, then concepts.** Start with a concrete problem: "We need X, but how?" Then introduce the concept as the solution. Abstract theory without motivation doesn't stick.

**Understand before implementing.** Before writing any code, the reader should understand WHY this approach works, what the alternatives are, and what trade-offs we're making. The goal is building mental models, not following steps.

**Active learning.** The reader should think and try things, not just read. Include:
- "Try this first before looking at the solution"
- "What do you think happens if...? Try it."
- "Open iex and inspect..."

**Struggle is learning.** Let the reader attempt things before showing the answer. Productive struggle builds deeper understanding than passive reading.

**Incremental building with verification.** Build code piece by piece, verifying each piece works. Never dump entire files. The complete file can appear at chapter end for reference.

**Depth over breadth in testing.** Write minimal tests focused on regressions (happy path, validation, security). Aim for 2-3 tests per chapter. But *explore* edge cases in the learning process even if you don't test them all.

## Tutorial Structure

Be specific to THIS codebase — no generic advice. Every recommendation should reference actual files, functions, and patterns that exist here. Use `path/to/file.ex:line_number` format when referencing code.

### Introduction

Start with:
- **The problem we're solving** — concrete, not abstract
- **What we'll learn** — the concepts and skills, not just the feature
- **Current state vs target state** — high-level architecture view
- **Complexity and risks** — what could go wrong

### Chapters

Organize into **chapters**, each focused on one coherent concept that can be understood, implemented, and verified before moving on.

Each chapter follows this structure:

#### 1. The Problem

Start with a concrete problem or question:
- "We need to store sites in the database, but how do we structure them?"
- "How does the user get from login to their site dashboard?"

Ground the concept in something tangible.

#### 2. Understanding (before any new code)

Build deep understanding:

**The concept:**
- What pattern/approach solves this problem?
- Why does this pattern exist in Phoenix/Ecto/OTP? What problem was it designed for?
- How does this codebase already use it? (Reference existing code: `lib/finde/accounts/user.ex:45`)

**How it actually works:**
- What happens under the hood when you call this function?
- What does Phoenix/Ecto do with this code?
- Encourage exploration: "Run `i Ecto.Changeset` in iex to see what a changeset contains"

**Trade-offs and alternatives:**
- Why this approach over alternatives?
- What are the downsides?
- When would you choose differently?

**Common mistakes:**
- What looks right but is subtly wrong?
- What error messages indicate which problems?

The reader should be able to explain the concept to someone else before writing code.

#### 3. Try It First (optional but encouraged)

Before showing the implementation:
- "Try writing the changeset function yourself based on what you learned above"
- "Predict what this code will do, then run it"
- "Implement a basic version, then compare with the solution below"

This is where deep learning happens—in the struggle.

#### 4. Build

Incremental implementation:
- Show small, digestible changes—not entire files
- Explain each piece as you add it
- Reference exact locations: `lib/finde/sites.ex:45`
- After each logical unit (a function, a module), verify it works

Example:
```
Create `lib/finde/sites/site.ex`. Start with the schema:

[schema code]

Before adding the changeset, open iex and try:
  iex> %Finde.Sites.Site{} |> Ecto.Changeset.change(%{title: "Test"})

See how we get a changeset even without validation? Now let's add proper validation:

[changeset code]

Try it again—what's different now?
```

#### 5. Verify

Prove it works before moving on:
- Console exploration: `iex> Sites.create_site(scope, %{...})`
- Minimal tests (2-3 per chapter) focused on: happy path, validation, security
- Manual browser checks for LiveViews
- Migration reversibility: `mix ecto.rollback && mix ecto.migrate`

#### 6. Go Deeper (optional)

For readers who want more:
- Edge cases: "What happens if the user submits twice quickly?"
- Internals: "Here's what `live_session` actually does under the hood..."
- Debugging: "If you see error X, it usually means Y"

### Transitions Between Chapters

At the end of each chapter:
- **Where we are:** Recap what we built and what we now understand
- **What's next:** Preview the next problem we'll solve
- **How it connects:** Show how this chapter's work fits into the bigger picture

### Wrapping Up

After all chapters:
- **The big picture:** How all the pieces fit together
- **What you now know:** Summarize the concepts mastered
- **Follow-up explorations:** Things to try, edge cases to explore, related concepts
- **Full test run:** `mix test` to verify everything works together

### Chapter Summary (at end of each chapter)

For chapters that create new files, include complete file contents for reference:

```
### Complete File: `lib/finde/sites/site.ex`

For verification, here's the complete file:
[contents]
```

## Output

1. Get today's date in `YYYY-MM-DD` format
2. Generate a short, kebab-case title (max 5-6 words) from the change description
3. Create the `history/` directory if it doesn't exist
4. Write the tutorial to: `history/YYYY-MM-DD-tutorial-<title>.md`

Example: `history/2025-12-13-tutorial-add-oauth-support.md`

## Tags

At the bottom of the tutorial document, add Bear-compatible tags for organization:

1. **Project tag:** Always include `#bytebottom/finde/tutorials`
2. **Technology tags:** Add tags for the main technology used in the tutorial, prefixed with `technology/`. Examples:
   - `#technology/elixir`
   - `#technology/phoenix`
   - `#technology/liveview`
   - `#technology/ecto`
   - `#technology/otp`
   - `#technology/tailwind`

**All tags must be lowercase.**

Example footer:
```
#bytebottom/finde/tutorials #technology/elixir #technology/phoenix #technology/liveview
```

Take your time. Be thorough. Teach, don't just instruct.
