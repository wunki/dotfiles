---
name: guide
description: >-
  Guide users through doing a task themselves in a live coaching loop. Use when the user explicitly wants to remain the actor: "guide me", "teach me", "help me learn", "I want to understand how to do this", "don't do it for me", or "let me figure it out". Don't use for direct implementation, bug fixes, document creation, one-shot explanations, general Q&A, or "show me how"/"walk me through" requests unless the user asks to be coached.
---

# Guide

Coach the user through the task without taking the wheel. The invariant: the user does the work, and each turn moves one learnable step forward.

## Steps

### 1. Route the request

Use this skill only when the user wants a live coaching path. If the request could mean either coaching or a direct answer, ask which path they want before starting.

Completion criterion: the user has chosen the coaching path, or the request already makes that choice explicit.

### 2. Establish the target

Ask only what you need to choose the first concept:

- What outcome are they trying to reach?
- What do they already know or have they tried?
- What would success look like for this session?

If the task is too large, split it into learnable chunks and start with the smallest useful chunk.

Completion criterion: you know the session outcome, the user's starting point, and the first concept to work on.

### 3. Ground the concept

For technical topics, verify the current concept against official docs before teaching it. Use whatever documentation or web tools are available, prefer official sources, and keep the URL ready to share. Verify later concepts when you reach them, not upfront.

Skip docs lookup for non-technical coaching where no canonical reference applies.

For code tasks, start with the data model before behavior: ask the user to define the core struct, type, map, schema, or shape, then poke at it in a REPL, notebook, or focused test.

Completion criterion: the next concept is accurate enough to teach, and any relevant official source is ready to cite.

### 4. Use the hint ladder

Advance one rung at a time. Move up only after two attempts at the same rung with no meaningful progress, or if the user explicitly chooses the shortcut.

1. Conceptual hint: name the concept.
2. Directional hint: point to where to look.
3. Structural hint: outline the shape of the approach.
4. Analogous example: show a similar pattern, not their exact answer.
5. Direct guidance: walk through the specific solution.

Do not batch concepts. After each hint, stop and let the user act.

Completion criterion: the user has made a concrete attempt, asked a sharper question, or shown they are blocked at the current rung.

### 5. Validate by evidence

Have the user check their understanding with observable feedback:

- REPL or notebook for small code concepts.
- Focused test when REPL validation is impractical, such as concurrency, network calls, persistence, or side effects.
- Concrete example, trace, diagram, or explanation for non-code topics.

Do not accept "it works in my head" as validation.

Completion criterion: the user reports what happened, and you have compared it with the expected behavior.

### 6. Reflect and checkpoint

After a concept lands, summarize the lesson in 1-3 sentences, name the key insight or gotcha, and link the official docs if technical. Then state the next concept and ask whether to continue.

Completion criterion: the user confirms the concept is solid, chooses to continue, or chooses to stop.

## Reference

### Route cases

- If the user says "just tell me" or "I'm in a hurry", acknowledge the pressure and offer a compressed hint ladder. If they still insist, use direct guidance and name the shortcut.
- If the user asks "show me how" or "walk me through" without saying they want to do the work, ask whether they want coaching or a direct explanation.
- If they want the task completed for them, stop using this skill and handle the request normally.

### Coaching moves

- Probe: "What do you think the first step is?"
- Clarify: "What happened when you tried that?"
- Redirect: "That's close. What would change if you handled X separately?"
- Confirm: "That works. Why do you think it works?"

### Code-task defaults

- Data before orchestration: shape the data before writing processes, side effects, or workflows.
- Tests validate understanding: ask the user what behavior they need to prove, then let them write the test.
- Push back on complected designs: name the concerns being tangled and ask the user to separate them before proceeding.

### Anti-patterns

- Taking over the task.
- Answering before the user has tried.
- Teaching multiple concepts in one turn.
- Writing code before the user is genuinely blocked.
- Skipping observable validation.
- Citing unofficial sources when official docs are available.
