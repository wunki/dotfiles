---
name: solveit
description: Generate a structured learning guide for building a feature using the Pólya problem-solving method. Creates a self-contained Markdown document that guides the user through understanding, planning, executing, and reflecting on the implementation. Triggers on phrases like "create a solveit guide", "make a learning guide for", "solveit for", "guided problem-solving for", or when the user wants a documented curriculum for building a feature rather than real-time guidance.
---

# Solveit Guide Generator

Generate a structured, self-contained learning document based on George Pólya's "How to Solve It" method. The output is a Markdown file that guides the reader to build a feature themselves.

**Key difference from Teach skill:** Solveit produces a complete document upfront; Teach is interactive real-time guidance.

## Core Principles

1. **The human is the agent** - The guide provides questions and structure; the reader does the thinking and coding
2. **Tiny increments** - 1-3 lines of code at a time, verified immediately
3. **Stop when unfamiliar** - Learning moments pause to explore new concepts
4. **Document thinking** - Reader writes answers down, not just thinks them

## Pólya's Four Phases

Every chapter follows this structure:

1. **Understand** - What is the unknown? The data? The conditions? Can you restate it?
2. **Plan** - Have you seen similar problems? Can you solve a simpler version first?
3. **Execute** - Build in tiny steps, verify each one, prove correctness
4. **Look Back** - Check the result, generalize, identify the key insight

## Workflow

### 1. Research First

Before writing the guide, thoroughly explore the codebase:
- Use Glob/Grep to find relevant files
- Read files that will be affected
- Trace data flow and dependencies
- Find similar implementations for reference

### 2. Generate the Guide

Create a document following the structure in [references/document-structure.md](references/document-structure.md).

Key elements:
- Opening with challenge, target state, and learning objectives
- Chapters following Pólya's 4 phases
- Hidden solutions using `<details>` tags
- Learning moments as blockquotes
- Verification commands after each tiny increment
- Progress checks between chapters
- Final reflection and acid test

### 3. Output Location

Save to: `history/YYYY-MM-DD-solveit-<kebab-case-title>.md`

Create `history/` directory if needed.

## Critical Rules

- **Maximum 1-3 lines per code increment** - Smaller than feels natural
- **Every increment has verification** - Exact command, exact expected output
- **Solutions are hidden** - Reader must try first
- **Be codebase-specific** - Reference actual files with `path/to/file.ex:line_number`
- **No generic advice** - Everything relates to THIS codebase

## References

- [Document Structure](references/document-structure.md) - Complete template with all sections
- [Formatting Patterns](references/formatting-patterns.md) - Code blocks, learning moments, details tags
