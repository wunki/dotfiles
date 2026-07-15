---
name: tech-docs-writer
description: >-
  Writes and rewrites technical documentation for software projects: README
  files, API references, architecture docs, user guides, and inline doc
  comments. Use when the user wants to create, improve, or restructure
  documentation for a codebase, library, CLI tool, or API. Also use when the
  user wants to audit existing docs for accuracy, clarity, or completeness.
  Don't use for commit messages, PR descriptions, changelogs, blog posts,
  marketing copy, or ad-hoc code comments that aren't part of a documentation
  system.
---

Technical documentation work follows this process. Apply writing principles throughout.

## Process

1. **Identify the doc type and audience.** If the user has not stated who reads this, infer from context (e.g., public API docs → external developers; internal architecture doc → the team). If audience is genuinely unclear, ask before writing.

2. **Read the source of truth.** Read the actual implementation files before writing anything. If no code is provided and none is accessible, state that explicitly and ask the user to share it. Do not document assumptions.

3. **Review existing documentation.** If docs already exist, read them first. Identify what is accurate, what is stale, and what is missing. Preserve deliberate style choices unless the user asks to change them.

4. **Find the lead.** Identify the one thing the reader must understand first. For a README: what the project does and why. For an API reference: what the endpoint does and its required inputs. Start there.

5. **Write the first draft.** Write to get structure down. Every section must earn its place.

6. **Rewrite for clarity.** Cut every word that does not work. Apply the language rules below.

7. **Verify.** Test every code example that can be tested. Check every file path. Run every command. For examples that cannot be run (e.g., require a live server or credentials), add a comment noting the prerequisite rather than omitting the verification step.

## Doc Type Conventions

**README**
- First paragraph: one sentence saying what the project does and why it exists.
- Quick start section: get the reader from zero to running in under 5 minutes.
- Structure: what it is → how to install → how to use → configuration → contributing.

**API Reference**
- One entry per endpoint or function: purpose, signature, parameters (name, type, required/optional, description), return value, error cases, and a minimal working example.
- Use placeholder credentials in examples (`YOUR_API_KEY`, `<token>`). Never use real values.

**Architecture Doc**
- Include a concise diagram (ASCII or Mermaid) showing components and data flow.
- Cover: component responsibilities, key interfaces between components, failure modes.
- Keep diagrams current with the implementation. If the code and diagram conflict, fix the diagram.

**User Guide**
- Task-oriented. Each section answers "how do I do X?" not "what is X?"
- Use numbered steps for sequential procedures. Use a decision tree when the path branches.

## Language Rules

- Active voice: "Run this command" not "This command should be run."
- Direct imperative: "Do this" not "You might want to consider doing this."
- Cut adverbs; the verb carries the weight.
- No jargon without definition; if a term must be used, earn it.
- Eliminate: "in order to" → "to", "utilize" → "use", "leverage" → "use", "functionality" → name what it does, "basically", "actually", "very."

## Code Examples

- Make them copy-pasteable and runnable by default.
- Include necessary imports and setup.
- Use realistic values, not `foo`/`bar`/`baz`.
- Use placeholder values for credentials: `YOUR_API_KEY`, `<token>`, `example.com`.
- Add a comment only where behavior is not obvious from reading the code.
- Test the example before including it. If it cannot be tested, note why inline.

## Formatting

- Backticks for inline code, file names, and command names.
- Fenced code blocks with language hints.
- Tables for structured comparisons (parameters, config options, error codes).
- Admonitions sparingly — if everything is a warning, nothing is.
- Avoid walls of unbroken prose: break at logical units.

## Quality Checklist

- [ ] First paragraph states what this is and why it matters.
- [ ] A reader can get started in under 5 minutes.
- [ ] Every code example has been tested or has an inline note explaining why it could not be.
- [ ] Every unnecessary word has been cut.
- [ ] The doc reads like a person talking, not a specification.
- [ ] No real credentials or secrets appear anywhere.
