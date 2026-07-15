---
name: code-simplifier
description: Refines existing code for clarity, readability, and maintainability without
  changing behavior, interfaces, or outputs. Use when asked to "simplify", "clean up",
  "refactor for readability", "reduce complexity", or polish code without adding features.
  Don't use when the request involves changing behavior, adding functionality, migrating
  to a new pattern or library, renaming public APIs, or any structural change that alters
  interfaces or outputs.
---

# Code Simplifier

Refine code so it is easier to read, reason about, and maintain, while preserving all behavior.

## Guardrails

1. Preserve functionality. Do not change outputs, side effects, public interfaces, or user-visible behavior.
2. Stay in scope. Default to the files the user named or recently modified. Ask before expanding scope.
3. Discover project conventions before editing: check for `AGENTS.md`, lint config, and existing code patterns in the target files. If none are found, follow the dominant style in the file.
4. Prefer explicit, maintainable code over clever compression.

## Workflow

1. Identify target files. If scope is ambiguous, ask the user to confirm which files or modules to touch before proceeding.
2. Find complexity hotspots:
   - Deep nesting or confusing control flow
   - Duplicate or redundant logic
   - Overly broad functions doing more than one thing
   - Dead code, unused parameters, or unnecessary abstractions
3. Simplify safely:
   - Extract helpers only when the extracted unit has a clear, stable name and the call site becomes meaningfully clearer. Do not extract for extraction's sake.
   - Replace deeply nested conditionals with early returns or guard clauses
   - Use descriptive names for values, functions, and boolean flags
   - Remove comments that only restate what the code already says
   - Apply language-idiomatic patterns, not patterns borrowed from other languages
4. Verify behavior is unchanged:
   - Run the relevant test suite or linter for touched files if a test command is known or discoverable from `package.json`, `Makefile`, or CI config.
   - If no tests exist, state that explicitly in the summary and flag it as a risk.
   - If tests fail after simplification, revert the change that caused the failure before proceeding.
5. If the code is already clean and no meaningful simplification is possible, say so explicitly. Do not manufacture changes.
6. Summarize only changes that had a clear complexity payoff. Note any assumptions made about intent or scope.

## Quality Bar

- The resulting code should be easier for a new contributor to modify correctly.
- If a simplification would increase ambiguity or risk, do not apply it.
- Favor root-cause cleanup over cosmetic edits.
