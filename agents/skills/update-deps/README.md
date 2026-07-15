# Update Deps

A Claude Code skill for safely updating Elixir dependencies in mix.exs. Handles patch/minor upgrades, major-version migrations, and verification.

## When to use

When asked to update deps, bump packages, or resolve outdated Mix/Hex dependencies.

## What it covers

- **Workflow**: How to assess, update, and verify dependency changes safely

## Files

- `SKILL.md` - Main skill definition
- `agents/openai.yaml` - OpenAI agent configuration for dep updates

## Usage

```
/update-deps
```
