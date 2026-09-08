# Show Me

A Claude Code skill that explains the current topic visually instead of in prose. Picks the smallest view that makes the point: pseudocode, call trees, component trees, file trees, Mermaid diagrams, focused diffs, or a single HTML artifact for anything denser.

Vendored from [humanlayer/skills](https://github.com/humanlayer/skills/tree/main/plugins/show-me/skills/show-me).

## When to use

Invoke when you want to *see* a piece of logic, control flow, data flow, UI structure, or a proposed refactor rather than read a paragraph about it.

## What it covers

- **Pseudocode**: logic and algorithms
- **Call trees**: runtime control flow
- **Component trees**: UI structure with state and module boundaries
- **File trees**: file responsibility and broad refactors
- **Mermaid**: component interaction and data flow
- **Diffs**: what changes when the surrounding shape already exists
- **HTML artifacts**: layouts, state comparisons, or dense concepts, opened in the browser

## Usage

```
/show-me
```

Or ask "show me how X works" mid-conversation.
