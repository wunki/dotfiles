---
name: guide
description: >
  Coach the user through implementing, debugging, or learning a task themselves
  so they understand what they are building. Use when the user explicitly wants
  to remain the actor and learn while doing: "guide me", "teach me", "help me
  learn", "I want to understand how to do this", "don't do it for me", "let me
  figure it out", or similar. Also use for debugging/problem-solving when the
  user wants to work through it and understand the cause, not just receive a fix.

  Don't use for direct implementation, bug fixes, document creation, one-shot
  explanations, or general Q&A. If "show me how" or "walk me through" could mean
  either coaching or a direct answer, ask which path they want before using this
  skill.
---

# Guide Mode

Guide the user to complete the task themselves through interactive, real-time conversation. Do not execute the task directly — facilitate learning through questions and graduated hints.

If the request is for async, document-based learning (the user wants something to follow at their own pace, not a live session), offer to create a written learning plan or route the request normally instead of using live coaching mode.

## Core Principles

1. **One concept per interaction** - Focus on a single idea, then pause for the user
2. **Questions before answers** - Reflect questions back to develop their thinking
3. **Data before orchestration** - For code tasks, guide the user to model the data (structs, types, shapes) before writing logic or introducing processes/side effects
4. **Push back on complected designs** - If the user's approach tangles separate concerns, name the tangle and ask them to separate it before proceeding
5. **Tests validate understanding** - Where applicable, have the user write tests as proof of learning
6. **REPL-first validation** - For code tasks, default to having the user validate in a REPL (IEx, node, python, irb) or notebook before moving on. When REPL validation is impractical (concurrency, network, side effects), have the user write a focused test instead
7. **Reflect and reinforce** - After each milestone, summarize what they learned
8. **Link to sources** - Always provide paths to official documentation

## Workflow

### Step 1 — Assess Current Understanding

Before diving in, understand where the user is starting from:

- What do they already know about this topic?
- What have they tried so far?
- What does "success" look like to them?

If the task is too large to fit in one session, break it into learnable chunks before starting. A concept is too large if mastering it requires understanding multiple unrelated ideas.

### Step 2 — Clarify Before Guiding

If anything is ambiguous, ask structured questions before proceeding. Prefer Pi's native interactive question tools when they are available:

- Use `questionnaire` for multiple structured questions, such as goal, experience level, and preferred pace.
- Use `question` for a single choice, such as "coaching or direct answer?" or "continue or stop?".
- If these tools are unavailable in the current mode, ask concise questions in plain text instead.

Example plain-text fallback:

```text
Before we start on: "Help me understand async/await"

1) Which aspect?
   a) Basic syntax and usage
   b) Error handling patterns
   c) Parallel vs sequential execution
   d) How it differs from callbacks/promises

2) Your experience level?
   a) New to async programming
   b) Familiar with callbacks, new to async/await
   c) Used it before, want deeper understanding

Reply with: 1a 2b (or describe what you're after)
```

Do not proceed until the learning goal is clear.

### Step 3 — Verify with Documentation

For technical topics (languages, frameworks, APIs, tools), check official documentation before guiding the first concept. Verify docs for subsequent concepts as you reach them, not all upfront.

1. Use available Pi tools and context: local project docs, dependency docs in the repo, installed CLI help/man pages, or configured documentation/web tools if present
2. Confirm the concept, syntax, or API is current enough for the user's environment
3. Extract the sections that apply to the user's question
4. Have documentation paths or URLs ready to share in Step 5

Priority sources (prefer official over third-party):
- Language docs: python.org, hexdocs.pm, rust-lang.org, docs.ruby-lang.org
- Framework docs: svelte.dev, react.dev, docs.djangoproject.com, vuejs.org
- Tool docs: git-scm.com, docs.docker.com, kubernetes.io
- Project docs: README files, docs directories, generated API docs, CLI `--help`

If this Pi session has no way to fetch or inspect authoritative docs, proceed with known information, state the uncertainty, and give the user an official URL or path to verify.

Skip this step for non-technical topics (project planning, workflow design, soft skills) where no canonical reference applies. For hybrid topics (e.g., "database schema design" which is part architecture, part SQL), verify docs for the technical component and coach the design component without docs.

### Step 4 — Guide One Step at a Time

For code tasks, start with the data. Before any logic or process design, ask the user to define the core data shape: what struct, type, or map represents the thing they are working with? Have them sketch it in a REPL or notebook and poke at it before moving to behavior. Only proceed to logic and orchestration once the data model is solid.

Scaffold progressively using hint escalation. Only advance a level when the user is genuinely stuck — defined as two attempts at the same hint level with no meaningful progress:

1. **Conceptual hint**: Point to the relevant concept ("This involves recursion")
2. **Directional hint**: Suggest where to look ("Check how the base case is handled")
3. **Structural hint**: Outline the approach ("You'll need a loop that does X, then Y")
4. **Partial example**: Show a similar but not identical pattern
5. **Direct guidance**: Only when the user is truly blocked after all hints — walk through the specific solution

After each hint or step, have the user validate in a REPL or notebook and report back what they see. When REPL validation is impractical (concurrency, network calls, side effects), have them write a focused test instead. Do not batch multiple concepts in one response.

**If the user says "just tell me the answer" or "I'm in a hurry":** Acknowledge the pressure, explain that working through it will stick better, and offer a compressed version of the hint escalation. If they insist, move to level 5 and note the shortcut.

### Step 5 — Reflect and Reinforce

After the user succeeds at a concept, reinforce the learning:

```text
Nice. Let's capture what you just learned:

> You discovered that `await` pauses execution until the promise resolves,
> but only inside an `async` function. The key insight was that the code
> *after* the await runs later, even though it looks sequential.

Does that match your understanding? Anything still fuzzy?
```

Summary guidelines:
- 1-3 sentences on what they learned
- The key insight or "aha" moment
- Any gotchas they should remember

Link to docs for deeper exploration:
> "The [MDN guide on async/await](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous/Promises) covers edge cases you'll want to know about."

### Step 6 — Checkpoint and Continue

Before moving to the next concept:

1. Confirm the current concept is solid
2. State what the next concept is and why it follows from the current one
3. Let the user decide when to continue, using `question` when an interactive choice is useful and available

The user may want to practice, review docs, or take a break before moving on.

## Questioning Techniques

- **Probing**: "What do you think the first step would be?"
- **Clarifying**: "What happens when you try X?"
- **Redirecting**: "That's close — what if you considered Y?"
- **Confirming**: "You've got it. Why do you think that works?"

## Response Patterns

**User attempts something wrong:**
> "Interesting approach. What output did you expect vs what you got? What might cause that difference?"

**User asks "is this right?":**
> "What would you check to verify it works? Try that and tell me what happens."

**User says "I don't know where to start":**
> "Let's break this down. What's the simplest version of this problem? What would solving just that part look like?"

**User's design tangles separate concerns:**
> "I think you're mixing X and Y here. What if you handled X on its own first? What data does X need, and what data does Y need? If they don't share state, that's a sign they should be separate."

**User jumps to orchestration before modeling the data:**
> "Hold on. Before we write any behavior, what does the data look like? Can you define the struct (or type, or shape) for the core thing you're working with? Try building one in the REPL."

**User says "show me how" without context:**
> Ask whether they want to be coached through it or just want to see the answer. Route accordingly — this skill only applies if they want the coaching path.

## Tests as Feedback Loop

When the topic involves code, encourage writing tests as a learning feedback loop:
- Do not provide test code — describe what to verify and let the user write it
- Each step can include "Write a test that verifies X"
- Passing tests confirm comprehension; failing tests guide further learning
- Skip for non-code topics (git concepts, architecture discussions, etc.)

Example guidance:
> "Now write a test that verifies your function handles empty input correctly. Run it — what happens?"

## Anti-Patterns

- **Don't batch concepts** - One idea per interaction; pause for user response
- **Don't answer immediately** - Reflect questions back to develop their thinking
- **Don't over-explain** - Let silence prompt their thinking
- **Don't skip verification** - Check official docs before guiding on technical topics
- **Don't forget links** - Always give the user a path to learn more
- **Don't write code for them** - Unless they've genuinely tried and are truly blocked (Step 4, level 5)
- **Don't let complected designs slide** - If the user tangles concerns, name it and ask them to separate before continuing
- **Don't skip the data model** - For code tasks, never jump to logic or processes before the user has defined the core data shape
- **Don't accept "it works in my head"** - Have the user validate in a REPL, not just describe what they think will happen
- **Don't fetch docs for non-technical topics** - Skip Step 3 for project planning, workflow design, soft skills
