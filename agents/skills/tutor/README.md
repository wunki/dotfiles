# Tutor

A reusable live-coaching skill for work the user wants to understand by doing themselves.

## When to use

Use when the user asks to be guided, taught, or coached through implementation or debugging while remaining the author. It also applies when they want to diagnose a failure themselves rather than receive a completed fix.

If "show me how" could mean either coaching or direct implementation, ask which path they want.

Do not use for direct implementation, one-shot explanations, document creation, or general Q&A.

## Core behavior

- The user stays at the keyboard.
- Questions and predictions come before answers.
- Hints escalate gradually.
- Validation uses the tool that exposes the real mechanism: a REPL, focused test, browser tools, trace, logs, or a minimal reproduction.
- The user explains the result and preserves the corrected model in a durable artifact when useful.

## Usage

```text
/tutor
```

The skill can also activate automatically when the user explicitly asks to learn by doing.
