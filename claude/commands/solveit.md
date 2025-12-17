---
description: Guide through building a feature using the Solveit/Pólya method
---

Think with maximum depth and thoroughness. Use extended thinking to its fullest capacity — take as long as needed.

## Task

Guide the developer through building a feature using the **Solveit method** — a learning-by-doing approach based on George Pólya's "How to Solve It" (1945). Do NOT implement the change yourself — create a guided problem-solving document that keeps the human as the agent.

**Feature to build:** $ARGUMENTS

---

## The Pólya Method

George Pólya identified four phases of problem-solving that have stood for 80 years. Every chapter follows this structure:

### 1. Understand the Problem

You cannot solve what you do not understand. Pólya's questions:

- **What is the unknown?** What are we trying to produce?
- **What are the data?** What do we have to work with?
- **What is the condition?** What constraints must be satisfied?
- **Can you restate the problem in your own words?**
- **Can you draw a diagram?**

The reader should be able to explain the problem to someone else before moving on.

### 2. Devise a Plan

Find the connection between the data and the unknown. Pólya's questions:

- **Have you seen this problem before?** Or in a slightly different form?
- **Do you know a related problem?** A problem with a similar unknown?
- **Can you use its method?** Can you use its result?
- **Can you restate the problem differently?**
- **Can you solve a simpler version first?** (specialization)
- **Can you solve part of it?** (decomposition)
- **Did you use all the data?** All the conditions?

The reader sketches their approach BEFORE seeing any code.

### 3. Carry Out the Plan

Execute in tiny increments, checking each step. Pólya's questions:

- **Can you see clearly that each step is correct?**
- **Can you prove it is correct?**

Build 1-3 lines at a time. Verify. Then continue.

### 4. Look Back

Examine the solution. This is where deep learning happens. Pólya's questions:

- **Can you check the result?** Does it satisfy the condition?
- **Can you derive the result differently?**
- **Can you use this result or method for other problems?**
- **What was the key insight?** The decisive idea?

---

## The Solveit Principles

These principles from Answer.AI's Solveit platform complement Pólya:

**The human is the agent.**
You are solving the problem. The guide provides questions, concepts, and verification — but YOU do the thinking. If you just read the solutions without trying, you've learned nothing.

**Tiny increments with immediate output.**
Write 1-3 lines of code. Run it. See the output. Verify it works. Then add more. Never write more than you can hold in your head at once. This is smaller than you think.

**Learning moments: STOP when something is unfamiliar.**
When you encounter something new, don't skip it. That creates cognitive debt. Stop. Explore it in iex. Understand it. Then continue. The discipline to stop is what separates learning from copying.

**Fast feedback catches misunderstandings early.**
Test immediately after each tiny increment. Bugs found in 3 lines are easy to fix. Bugs found in 30 lines waste hours.

**Document your thinking.**
Write down your answers to the questions. Not in your head — on paper or in a file. The act of writing clarifies thought.

---

## Research Phase (Do This First)

Before writing the guide, thoroughly explore the codebase:
- Use Glob and Grep to find all relevant files
- Read the files that will be affected
- Trace data flow and dependencies
- Understand existing patterns and conventions
- Look at similar implementations for reference

You must deeply understand the system to guide someone else through it.

---

## Document Structure

Be specific to THIS codebase — no generic advice. Reference actual files with `path/to/file.ex:line_number`.

### Opening

```markdown
# Solveit: [Feature Name]

**Date:** YYYY-MM-DD
**Complexity:** Small / Medium / Large
**Time:** [Estimated focused time, e.g., "1-2 hours"]

---

## Before You Begin

This is not a tutorial to copy. It's a **guided problem-solving session**.

You will:
- Answer questions before seeing solutions
- Plan before you code
- Build in tiny increments (1-3 lines)
- Verify constantly
- Reflect on what you learned

**Keep a scratchpad open.** Write down your answers to the questions. The act of writing clarifies thought.

**Your job is to solve the problem.** This guide keeps you on track.

---

## The Challenge

[State the problem in 2-3 sentences. Concrete and tangible. What does the user need? Why does it matter?]

## What You'll Build

[Show the target state. What files will exist? How will data flow? A simple diagram or file list.]

## What You'll Learn

By the end, you'll deeply understand:
- [Concept 1]
- [Concept 2]
- [Concept 3]
```

---

### Chapters

Each chapter is one coherent problem to solve. Follow Pólya's four steps faithfully.

---

#### Step 1: Understand

```markdown
## Chapter N: [Title]

### 1. Understand the Problem

**The problem:**
[One paragraph. What specifically needs to happen?]

---

**Pólya's questions** — write your answers down:

1. **What is the unknown?** What are we trying to create or compute?
2. **What are the data?** What do we have to work with?
3. **What is the condition?** What must be true for the solution to be correct?
4. **Can you draw it?** Sketch the data flow, the UI, or the structure.
5. **Can you restate it?** Explain the problem in your own words.

Take 2-3 minutes. Write it down. Don't skip this.

---

**Relevant concepts:**

[Explain ONLY the Phoenix/Ecto/OTP patterns needed for this chapter. Reference existing code in this codebase that demonstrates the pattern.]

Example: "Phoenix contexts group related functionality behind a clean API. Look at how `lib/finde/accounts.ex:27-29` exposes `get_user_by_email/1` while hiding the Repo details."

**⏸️ Learning Moment** (when introducing something unfamiliar):

> **Stop if this is new to you.** The `%Scope{}` pattern might be unfamiliar.
>
> Open iex and explore:
> ```elixir
> iex> alias Finde.Accounts.Scope
> iex> scope = Scope.for_user(%Finde.Accounts.User{id: 1})
> iex> scope.user
> ```
>
> What does the Scope contain? Why might we pass it to every function?
>
> Understand it before continuing. Skipping creates debt.
```

**Key principle:** The reader should be able to explain the problem to someone else before moving to Plan.

---

#### Step 2: Plan

```markdown
### 2. Devise a Plan

**Pólya's questions** — write your answers down:

1. **Have you seen a similar problem?** Where in this codebase? What approach did it use?
2. **Can you use that method here?**
3. **Can you solve a simpler version first?** (e.g., without validation, without edge cases)
4. **Can you solve part of it?** What's the first piece?
5. **What functions will you need?** Sketch the signatures.
6. **Did you use all the data? All the conditions?**

Take 3-5 minutes. Sketch your approach. Write it down.

---

<details>
<summary>One possible approach (try first, then look)</summary>

[Show the plan — NOT code, just structure]

Example:
> **Files to create:**
> - `lib/finde/sites.ex` — Context module with scoped queries
> - `lib/finde/sites/site.ex` — Schema with UXID primary key
>
> **Functions needed:**
> - `list_sites(scope)` → returns `[%Site{}]`
> - `create_site(scope, attrs)` → returns `{:ok, %Site{}}` or `{:error, changeset}`
> - `get_site!(scope, id)` → returns `%Site{}` or raises
>
> **Key decisions:**
> - UXID for IDs (URL-friendly, K-sortable)
> - Scope pattern for user isolation
> - Slug auto-generated from title

</details>

**Compare with your plan:**
- What's different?
- What did you miss?
- What trade-offs are being made?
```

**Key principle:** The reader has a clear plan before writing any code.

---

#### Step 3: Execute

```markdown
### 3. Carry Out the Plan

Now build it — in tiny steps.

**Rules:**
- Write 1-3 lines at a time
- Verify after each increment
- If something's unfamiliar, STOP and explore it
- Ask yourself: "Can I prove this step is correct?"

---

**Step 3.1: [First tiny piece]**

[Describe what to build in one sentence]

Try it yourself first. Then compare:

<details>
<summary>Solution</summary>

```elixir
[1-3 lines ONLY]
```

</details>

**Verify immediately:**
```
iex> [exact command]
[exact expected output]
```

✓ Does yours match? If not, debug now. Don't continue with broken code.

---

**Step 3.2: [Next tiny piece]**

[Same pattern]

---

**⏸️ Learning Moment** (inline, when new concepts appear):

> We just used `Repo.exists?/1`. Stop and explore:
> ```
> iex> h Repo.exists?
> ```
> Why is this better than `Repo.all() |> Enum.empty?()`?
>
> Answer before continuing: _______________

---

**Step 3.3: [Continue...]**

[Continue until the chapter's goal is achieved]
```

**Critical rules for Execute:**

1. **Maximum 1-3 lines per increment.** If you're showing more, break it down. This is smaller than feels natural. That's the point.

2. **Every increment has verification.** Show the exact command and exact expected output.

3. **Solutions are hidden.** Use `<details>` tags. The reader must try first.

4. **Learning moments are inline.** When something new appears, stop right there. Don't defer to a later section.

5. **Ask "Can I prove this is correct?"** Each step should have a clear reason it works.

---

#### Step 4: Look Back

```markdown
### 4. Look Back

You've built [what]. Now examine your solution — this is where deep learning happens.

**Pólya's questions** — write your answers:

1. **Can you check the result?** Does it satisfy all the conditions from Step 1?
   ```
   iex> [verification command]
   [expected output proving it works]
   ```

2. **Can you derive it differently?** What's another way to implement this?

3. **Can you use this method elsewhere?** What other problems could this approach solve?

4. **What was the key insight?** The decisive idea that made it work?

---

**Self-assessment** — be honest:

- [ ] I can explain why each function is structured this way
- [ ] I can predict what happens if I change [key piece]
- [ ] I could build this again without looking at the guide
- [ ] I could apply this pattern to a different problem

If you can't check all boxes, go back and review.

---

**Where we are:**
[One paragraph: what we built, what we now understand, how it connects to the bigger picture]

**What's next:**
[One sentence: the next problem to solve]
```

**Key principle:** Reflection isn't optional. It's where learning solidifies.

---

### When You're Stuck

Include this section in the guide for when things aren't working:

```markdown
## If You're Stuck

Pólya's advice for when you can't make progress:

1. **Go back to the problem.** Did you understand it fully? Restate it.
2. **Go back to your plan.** Is it complete? Did you use all the data?
3. **Try a simpler case.** Can you solve an easier version first?
4. **Try a special case.** What if there's only one item? Zero items?
5. **Work backwards.** Start from the goal. What would produce that?
6. **Check your definitions.** Do you really understand what each function does?
7. **Take a break.** Walk away. Come back fresh.

If you're stuck on a specific error:
```
iex> [command that produced the error]
** (SomeError) [error message]
```

Read the error message carefully. It usually tells you exactly what's wrong.
```

---

### Transitions

Between chapters, show progress and connection:

```markdown
---

## Progress Check

**Built so far:**
- ✅ Chapter 1: [Component] — [what it does]
- ✅ Chapter 2: [Component] — [what it does]
- 🔲 Chapter 3: [Component] — [what we'll build]

**How the pieces connect:**
[Data flow or architectural description showing how completed work fits together]

---
```

---

### Wrapping Up

```markdown
## You Solved It

### What You Built

[List the components]

### The Key Insights

Review your notes. The decisive ideas were:

1. **[Concept 1]:** [One sentence — the key insight]
2. **[Concept 2]:** [One sentence — the key insight]
3. **[Concept 3]:** [One sentence — the key insight]

### Can You Use This Elsewhere?

The patterns you learned apply to:
- [Similar problem 1]
- [Similar problem 2]

### The Acid Test

- [ ] I can build something similar without this guide
- [ ] I can explain each component to someone else
- [ ] I can debug issues without help
- [ ] I can extend this to new requirements

All boxes checked? You own this knowledge.

### Go Deeper

Things to explore:
- [Edge case to try]
- [Alternative approach]
- [Related concept]

### Final Verification

```bash
mix test
```

All green? You're done.
```

---

### Reference Files

At the very end only:

```markdown
---

## Reference: Complete Files

For verification only. If you built along, yours should match.

### `lib/finde/sites.ex`

```elixir
[complete file]
```
```

---

## Formatting Guidelines

### Hide Solutions

```markdown
<details>
<summary>Solution</summary>

```elixir
# 1-3 lines max
```

</details>
```

### Learning Moments Use Blockquotes

```markdown
> **⏸️ Stop if this is new.** The pipe operator `|>` passes the left result as the first argument to the right.
>
> Try it:
> ```
> iex> "hello" |> String.upcase()
> ```
>
> Understand it before continuing.
```

### Keep Code Tiny

**Too big (don't do this):**
```elixir
def create_site(%Scope{user: user}, attrs \\ %{}) do
  %Site{user_id: user.id}
  |> Site.changeset(attrs)
  |> Repo.insert()
end
```

**Right size (do this):**
```elixir
%Site{user_id: user.id}
```

Verify. Then add the next line:
```elixir
|> Site.changeset(attrs)
```

Verify. Then:
```elixir
|> Repo.insert()
```

Verify. Three increments, not one.

---

## Output

1. Get today's date in `YYYY-MM-DD` format
2. Generate a short, kebab-case title (max 5-6 words)
3. Create `history/` if needed
4. Write to: `history/YYYY-MM-DD-solveit-<title>.md`
5. Ask the user if they want to open the file in their Markdown viewer: `open <filepath>`

## Tags

```
#bytebottom/finde/solveit #technology/elixir #technology/phoenix [etc.]
```

All lowercase.

---

## Remember

You are creating a **guided problem-solving experience** based on Pólya's method.

The reader should:
- **Understand** before planning (Pólya's questions)
- **Plan** before coding (sketch the approach)
- **Execute** in tiny increments (1-3 lines, verify each)
- **Look Back** to consolidate (check, generalize, reflect)

**The human is the agent.** They solve. You guide.

**Document your thinking.** Written answers, not mental ones.

**Stop when something is unfamiliar.** Skipping creates debt.

Take your time. Be thorough. Guide, don't instruct.
