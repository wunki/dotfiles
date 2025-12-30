---
name: beads
description: >
  Tracks complex, multi-session work using the Beads issue tracker and dependency graphs, and provides
  persistent memory that survives conversation compaction. Use when work spans multiple sessions, has
  complex dependencies, or needs persistent context across compaction cycles. Trigger with phrases like
  "create task for", "what's ready to work on", "show task", "track this work", "what's blocking", or
  "update status".
---

# Beads - Persistent Task Memory for AI Agents

Graph-based issue tracker that survives conversation compaction. Provides persistent memory for multi-session work with complex dependencies.

## Overview

**bd (beads)** replaces markdown task lists with a dependency-aware graph stored in git. Unlike TodoWrite (session-scoped), bd persists across compactions and tracks complex dependencies.

**Key Distinction**:
- **bd**: Multi-session work, dependencies, survives compaction, git-backed
- **TodoWrite**: Single-session tasks, linear execution, conversation-scoped

**Core Capabilities**:
- Dependency Graphs: Track what blocks what (blocks, parent-child, discovered-from, related)
- Compaction Survival: Tasks persist when conversation history is compacted
- Git Integration: Issues versioned in `.beads/issues.jsonl`, sync with `bd sync`
- Smart Discovery: Auto-finds ready work (`bd ready`), blocked work (`bd blocked`)
- Audit Trails: Complete history of status changes, notes, and decisions
- Rich Metadata: Priority (P0-P4), types (bug/feature/task/epic), labels, assignees

**When to Use bd vs TodoWrite**:
- "Will I need this context in 2 weeks?" -> YES = bd
- "Could conversation history get compacted?" -> YES = bd
- "Does this have blockers/dependencies?" -> YES = bd
- "Is this fuzzy/exploratory work?" -> YES = bd
- "Will this be done in this session?" -> YES = TodoWrite
- "Is this just a task list for me right now?" -> YES = TodoWrite

**Decision Rule**: If resuming in 2 weeks would be hard without bd, use bd.

## Prerequisites

**Required**:
- **bd CLI**: Version 0.34.0 or later installed and in PATH
- **Git Repository**: Current directory must be a git repo
- **Initialization**: `bd init` must be run once (humans do this, not agents)

**Verify Installation**:
```bash
bd --version  # Should return 0.34.0 or later
```

## Session Start Protocol

**Every session, start here:**

### Step 1: Check for Ready Work

```bash
bd ready
```

Shows tasks with no open blockers, sorted by priority (P0 -> P4).

### Step 2: Pick Highest Priority Task

Choose the highest priority (P0 > P1 > P2 > P3 > P4) task that's ready.

### Step 3: Get Full Context

```bash
bd show <task-id>
```

Displays full task description, dependency graph, audit trail, and metadata.

### Step 4: Start Working

```bash
bd update <task-id> --status in_progress
```

### Step 5: Add Notes as You Work

```bash
bd update <task-id> --notes "Completed: X. In progress: Y. Blocked by: Z"
```

**Critical for compaction survival**: Write notes as if explaining to a future agent with zero conversation context.

**Note Format** (best practice):
```
COMPLETED: Specific deliverables (e.g., "implemented JWT refresh endpoint + rate limiting")
IN PROGRESS: Current state + next immediate step
BLOCKERS: What's preventing progress
KEY DECISIONS: Important context or user guidance
```

## Task Creation Workflow

### When to Create Tasks

Create bd tasks when:
- User mentions tracking work across sessions
- User says "we should fix/build/add X"
- Work has dependencies or blockers
- Exploratory/research work with fuzzy boundaries

### Basic Task Creation

```bash
bd create "Task title" -p 1 --type task
```

**Arguments**:
- **Title**: Brief description (required)
- **Priority**: 0-4 where 0=critical, 1=high, 2=medium, 3=low, 4=backlog (default: 2)
- **Type**: bug, feature, task, epic, chore (default: task)

### Create with Description

```bash
bd create "Implement OAuth" -p 1 --description "Add OAuth2 support for Google, GitHub, Microsoft."
```

### Epic with Children

```bash
# Create parent epic
bd create "Epic: OAuth Implementation" -p 0 --type epic
# Returns: myproject-abc

# Create child tasks
bd create "Research OAuth providers" -p 1 --parent myproject-abc
bd create "Implement auth endpoints" -p 1 --parent myproject-abc
```

## Update & Progress Workflow

### Change Status

```bash
bd update <task-id> --status <new-status>
```

**Status Values**: `open`, `in_progress`, `blocked`, `closed`

### Add Progress Notes

```bash
bd update <task-id> --notes "Progress update here"
```

**Appends** to existing notes field (doesn't replace).

### Change Priority

```bash
bd update <task-id> -p 0  # Escalate to critical
```

### Add Labels

```bash
bd label add <task-id> backend
bd label add <task-id> security
```

## Dependency Management

### Add Dependencies

```bash
bd dep add <child-id> <parent-id>
```

**Meaning**: `<parent-id>` blocks `<child-id>` (parent must be completed first).

**Dependency Types**: blocks, parent-child, discovered-from, related

### View Dependencies

```bash
bd dep list <task-id>
```

## Completion Workflow

### Close a Task

```bash
bd close <task-id> --reason "Completion summary"
```

**Best Practice**: Always include a reason describing what was accomplished.

### Check Newly Unblocked Work

After closing a task, run:

```bash
bd ready
```

Closing a task may unblock dependent tasks, making them newly ready.

## Git Sync Workflow

### All-in-One Sync

```bash
bd sync
```

**Performs**: Export, commit, pull, merge, import, push.

**Use when**: End of session, before handing off, after major progress.

## Essential Commands Quick Reference

| Command | Purpose |
|---------|---------|
| `bd ready` | Show tasks ready to work on |
| `bd create "Title" -p 1` | Create new task |
| `bd show <id>` | View task details |
| `bd update <id> --status in_progress` | Start working |
| `bd update <id> --notes "Progress"` | Add progress notes |
| `bd close <id> --reason "Done"` | Complete task |
| `bd dep add <child> <parent>` | Add dependency |
| `bd list` | See all tasks |
| `bd search <query>` | Find tasks by keyword |
| `bd sync` | Sync with git remote |
| `bd blocked` | Find stuck work |
| `bd stats` | Project metrics |

## Complete Command Reference

| Command | When to Use |
|---------|-------------|
| **FIND COMMANDS** | |
| `bd ready` | Find unblocked tasks |
| `bd list` | View all tasks (with filters) |
| `bd show <id>` | Get task details |
| `bd search <query>` | Text search across tasks |
| `bd blocked` | Find stuck work |
| `bd stats` | Project metrics |
| **CREATE COMMANDS** | |
| `bd create` | Track new work |
| `bd template create` | Use issue template |
| **UPDATE COMMANDS** | |
| `bd update <id>` | Change status/priority/notes |
| `bd dep add` | Link dependencies |
| `bd label add` | Tag with labels |
| `bd comments add` | Add comment |
| `bd reopen <id>` | Reopen closed task |
| **COMPLETE COMMANDS** | |
| `bd close <id>` | Mark task done |
| `bd epic close-eligible` | Auto-close complete epics |
| **SYNC COMMANDS** | |
| `bd sync` | Git sync (all-in-one) |
| `bd export` | Export to JSONL |
| `bd import` | Import from JSONL |
| `bd daemon` | Background sync manager |

## Error Handling

### Common Failures

1. **`bd: command not found`**: bd CLI not installed. Install from https://github.com/steveyegge/beads
2. **`No .beads database found`**: Run `bd init` (humans do this once)
3. **`Task not found: <id>`**: Use `bd list` to verify ID
4. **`Circular dependency detected`**: bd prevents cycles automatically
5. **Git merge conflicts**: Use `bd sync --merge` for auto-resolution

## Examples

### Multi-Session Feature (Epic with Children)

```bash
# Create epic
bd create "Epic: OAuth Implementation" -p 0 --type epic
# Returns: project-abc

# Create child tasks
bd create "Research OAuth providers" -p 1 --parent project-abc
bd create "Implement backend auth endpoints" -p 1 --parent project-abc
bd create "Add frontend login UI" -p 2 --parent project-abc

# Add dependencies (backend before frontend)
bd dep add project-abc.3 project-abc.2

# Start with research
bd update project-abc.1 --status in_progress
```

### Tracking Blocked Work

```bash
# Mark current task as blocked
bd update project-xyz --status blocked --notes "API endpoint /auth returns 503"

# Create blocker task
bd create "Fix /auth endpoint 503 error" -p 0 --type bug

# Link dependency
bd dep add project-xyz project-blocker

# Find other ready work
bd ready
```

### Session Resume After Compaction

```bash
# Session 2 (weeks later):
bd ready
# Shows: myproject-auth [P1] [task] in_progress

bd show myproject-auth
# Full context preserved - no conversation history needed!

# Continue exactly where you left off
bd update myproject-auth --notes "COMPLETED: Token refresh. IN PROGRESS: Rate limiting"
```

## Resources

Full documentation: https://github.com/steveyegge/beads
