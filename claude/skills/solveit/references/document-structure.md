# Solveit Document Structure

## Opening

```markdown
# Solveit: [Feature Name]

**Date:** YYYY-MM-DD
**Complexity:** Small / Medium / Large
**Time:** [Estimated focused time, e.g., "1-2 hours"]

---

## The Challenge

[State the problem in 2-3 sentences. Concrete and tangible.]

## What You'll Build

[Show the target state. What files will exist? How will data flow?]

## What You'll Learn

By the end, you'll deeply understand:
- [Concept 1]
- [Concept 2]
- [Concept 3]
```

---

## Chapter Structure

Each chapter is one coherent problem following Pólya's four steps.

### Step 1: Understand

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

[Explain ONLY the patterns needed for this chapter. Reference existing code.]

Example: "Phoenix contexts group related functionality. Look at how `lib/app/accounts.ex:27-29` exposes `get_user_by_email/1` while hiding Repo details."

**⏸️ Learning Moment** (when introducing something unfamiliar):

> **Stop if this is new to you.** The `%Scope{}` pattern might be unfamiliar.
>
> Open iex and explore:
> ```elixir
> iex> alias App.Accounts.Scope
> iex> scope = Scope.for_user(%App.Accounts.User{id: 1})
> ```
>
> What does the Scope contain? Understand it before continuing.
```

### Step 2: Plan

```markdown
### 2. Devise a Plan

**Pólya's questions** — write your answers down:

1. **Have you seen a similar problem?** Where in this codebase?
2. **Can you use that method here?**
3. **Can you solve a simpler version first?**
4. **Can you solve part of it?** What's the first piece?
5. **What functions will you need?** Sketch the signatures.
6. **Did you use all the data? All the conditions?**

Take 3-5 minutes. Sketch your approach. Write it down.

---

<details>
<summary>One possible approach (try first, then look)</summary>

**Files to create:**
- `lib/app/sites.ex` — Context module
- `lib/app/sites/site.ex` — Schema

**Functions needed:**
- `list_sites(scope)` → returns `[%Site{}]`
- `create_site(scope, attrs)` → returns `{:ok, %Site{}}` or `{:error, changeset}`

</details>

**Compare with your plan:**
- What's different?
- What did you miss?
```

### Step 3: Execute

```markdown
### 3. Carry Out the Plan

Now build it — in tiny steps.

**Rules:**
- Write 1-3 lines at a time
- Verify after each increment
- If something's unfamiliar, STOP and explore it

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

✓ Does yours match? If not, debug now.

---

**Step 3.2: [Next tiny piece]**

[Same pattern - continue until chapter goal is achieved]
```

### Step 4: Look Back

```markdown
### 4. Look Back

You've built [what]. Now examine your solution.

**Pólya's questions** — write your answers:

1. **Can you check the result?** Does it satisfy all the conditions?
   ```
   iex> [verification command]
   [expected output]
   ```

2. **Can you derive it differently?** What's another way?

3. **Can you use this method elsewhere?**

4. **What was the key insight?**

---

**Self-assessment** — be honest:

- [ ] I can explain why each function is structured this way
- [ ] I can predict what happens if I change [key piece]
- [ ] I could build this again without the guide
- [ ] I could apply this pattern to a different problem

If you can't check all boxes, go back and review.

---

**Where we are:**
[What we built, what we understand, how it connects]

**What's next:**
[The next problem to solve]
```

---

## Between Chapters

```markdown
---

## Progress Check

**Built so far:**
- ✅ Chapter 1: [Component] — [what it does]
- ✅ Chapter 2: [Component] — [what it does]
- 🔲 Chapter 3: [Component] — [what we'll build]

**How the pieces connect:**
[Data flow or architectural description]

---
```

---

## If You're Stuck Section

Include once in the guide:

```markdown
## If You're Stuck

Pólya's advice:

1. **Go back to the problem.** Restate it.
2. **Go back to your plan.** Is it complete?
3. **Try a simpler case.** One item? Zero items?
4. **Work backwards.** Start from the goal.
5. **Check your definitions.** Do you really understand each function?
6. **Take a break.** Come back fresh.
```

---

## Closing

```markdown
## You Solved It

### What You Built

[List the components]

### The Key Insights

1. **[Concept 1]:** [One sentence]
2. **[Concept 2]:** [One sentence]

### The Acid Test

- [ ] I can build something similar without this guide
- [ ] I can explain each component to someone else
- [ ] I can debug issues without help
- [ ] I can extend this to new requirements

All boxes checked? You own this knowledge.

### Final Verification

```bash
mix test
```

All green? You're done.

---

## Reference: Complete Files

For verification only. If you built along, yours should match.

### `lib/app/example.ex`

```elixir
[complete file]
```
```
