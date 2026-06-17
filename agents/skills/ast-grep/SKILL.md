---
name: ast-grep
description: Use ast-grep for syntax-aware structural code search, analysis, linting, and safe rewrite planning. Load this when a task asks to find code by shape/AST, refactor repeated code patterns, inspect function/component/import usage structurally, write ast-grep rules, or when ripgrep would be too imprecise.
---

Use `ast-grep` (`sg`) when the task depends on code structure: calls, imports, functions, components, conditionals, object literals, nested constructs, or safe repeated rewrites.

Prefer `rg` for plain text/content search.

## Default commands

Check availability:

```bash
ast-grep --version || sg --version
```

Search by AST pattern:

```bash
ast-grep run --lang <language> -p '<pattern>' [path]
# short form when sg is installed:
sg run --lang <language> -p '<pattern>' [path]
```

Use JSON when output may be parsed or large:

```bash
ast-grep run --lang ts -p 'console.log($$$ARGS)' --json=stream
```

Preview rewrites before applying:

```bash
ast-grep run --lang js -p 'var $NAME = $VALUE' -r 'let $NAME = $VALUE'
ast-grep run --lang js -p 'var $NAME = $VALUE' -r 'let $NAME = $VALUE' --update-all
```

Only use `--update-all` after inspecting the preview.

## Pattern reminders

- Patterns should look like real source code in the target language.
- `$A` captures one AST node.
- `$$$ARGS` captures zero or more nodes.
- Always set `--lang` explicitly (`js`, `ts`, `jsx`, `tsx`, `json`, `yaml`, `python`, `go`, `rust`, etc.).
- Quote patterns with single quotes to avoid shell expansion.
- If a pattern fails unexpectedly, inspect it with `--debug-query=ast`.

## Common patterns

```bash
# console calls
ast-grep run --lang ts -p 'console.log($$$ARGS)'

# function declarations
ast-grep run --lang ts -p 'function $NAME($$$ARGS) { $$$BODY }'

# React hook usage
ast-grep run --lang tsx -p 'useEffect($$$ARGS)'

# imports from a package
ast-grep run --lang ts -p 'import { $$$IMPORTS } from "$PKG"'

# YAML key/value shape
ast-grep run --lang yaml -p '$KEY: $VALUE'
```

## Rule workflow

For complex searches or reusable rules:

1. Start from a small code example that should match.
2. Write the simplest working `-p` pattern first.
3. Add YAML rule constraints only after the basic pattern works.
4. Use `--json=stream` and/or `--debug-query=ast` to inspect matches.
5. If it fails, remove constraints and debug one condition at a time.
6. Preview rewrites; apply only after reviewing matches.

## Docs to fetch when needed

- AI prompting: https://ast-grep.github.io/advanced/prompting.html
- LLM docs: https://ast-grep.github.io/llms-full.txt
- Pattern syntax: https://ast-grep.github.io/guide/pattern-syntax.html
- Rule config: https://ast-grep.github.io/guide/rule-config.html
- JSON output: https://ast-grep.github.io/guide/tools/json.html
- Rewrite code: https://ast-grep.github.io/guide/rewrite-code.html
- CLI reference: https://ast-grep.github.io/reference/cli.html
