---
name: ship
description: Create a SHIP.md document - a concrete plan for finishing a project. Analyzes the codebase, defines what "done" means, breaks work into phases with small steps. No motivational fluff - just the plan. Triggers on "help me ship this", "create a ship document", "help me finish this project", "I'm stuck on this project", or when a user needs a clear completion plan.
---

# Ship

Create a **SHIP.md** document—a concrete plan for finishing a project. Not a pep talk. Just: what's done, what's left, what order, how to track it.

## Process

1. **Analyze** - Read README, docs, code structure
2. **Assess state** - What's built? What remains?
3. **Define "done"** - Minimum shippable version
4. **Break into phases** - 3-5 phases, small concrete steps
5. **Write SHIP.md**

## Structure Principles

The behavioral science is in the *structure*, not the prose:

- **Small steps** (completable in 25-60 min) - reduces overwhelm
- **Visible progress** (X/Y counter) - creates momentum
- **Explicit scope cuts** - prevents scope creep
- **Ship date field** - commitment device
- **"Start now" ending** - immediate action bias

## SHIP.md Template

```markdown
# [Project Name]: Ship Plan

## Done
What's already working. Acknowledge progress.

## Target
One sentence. What "shipped" means.

## Out of Scope
Features explicitly not included in this version.

## Phases

### Phase 1: [Name] (~X hours)
- [ ] Step 1
- [ ] Step 2
- [ ] Step 3

### Phase 2: [Name] (~X hours)
- [ ] Step 4
- [ ] Step 5

### Phase 3: [Name] (~X hours)
- [ ] Step 6
- [ ] Step 7

## Ship Date
_______________

## Progress
**___ / [total] steps**

## Next Step
[The single next action to take right now]
```

## Guidelines

- Steps should be concrete actions, not vague goals
- Each step completable in one focused session
- No explanations of why you're procrastinating
- No motivational framing
- No "how you'll feel" predictions
- Just the work, broken down

## Output

Create `SHIP.md` in project root.
