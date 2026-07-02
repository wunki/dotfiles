# Guide

A Pi-compatible skill for live coaching when the user wants to implement, debug, or learn a task themselves and understand what they are building.

## When to use

Use when the user explicitly wants to stay in the driver's seat: "guide me", "teach me", "help me learn", "I want to understand how to do this", "don't do it for me", or "let me figure it out".

Also use for debugging or problem-solving when the user wants to work through the cause themselves, not just receive a fix.

If the user says "show me how" or "walk me through" without saying they want coaching, ask whether they want live coaching or a direct answer.

Don't use for direct implementation, bug fixes, document creation, one-shot explanations, or general Q&A.

## Core behavior

- One concept per turn.
- Questions and hints before answers.
- Data model before code orchestration.
- Official/project docs via available Pi tools.
- Pi `question`/`questionnaire` tools for structured coaching prompts when available.
- REPL, focused test, or concrete evidence before moving on.
- Short reflection after each concept so the user can name what they learned.

## Usage

```text
/guide
```
