---
model: claude-sonnet-4-5-20250929
description: List all git worktrees with their configuration and status
allowed-tools: Bash, Read, Glob, Grep
---

# Purpose

List all git worktrees in the `trees/` directory with comprehensive information including branch names, directories, port configuration, and service status.

## Variables

```
PROJECT_CWD: . (current working directory - the main project root)
WORKTREE_BASE_DIR: trees/
BASE_PORT: 4000
```

## Instructions

- List all worktrees managed by git
- For each worktree in trees/, gather configuration details
- Read environment files to extract port configuration
- Check if services are running on configured ports
- Display comprehensive information in a clear, organized format
- Show which worktrees are active vs stopped
- Provide quick action commands for each worktree

## Workflow

### 1. List Git Worktrees

- Run: `git worktree list`
- Parse output to identify all worktrees
- Filter for worktrees in PROJECT_CWD/trees/ directory
- Extract:
  - Worktree path
  - Branch name
  - Commit hash (if available)

### 2. Gather Configuration for Each Worktree

For each worktree found in trees/:

**Extract Branch/Directory Info:**
- Worktree directory name (slug)
- Branch name from git worktree list
- Working directory path

**Read Configuration:**
- Check if `<worktree>/.env` exists
- If exists, read and extract `PORT`
- If doesn't exist, note as "Not configured"

**Read Database Configuration:**
- Check if `<worktree>/config/dev.local.exs` exists
- If exists, extract database name
- If doesn't exist, note as "Using default database"

### 3. Check Service Status

For each worktree with port configuration:

- If PORT identified, check: `lsof -i :<PORT>`
- Determine if process is running
- Extract PID if running

### 4. Check Dependencies

For each worktree:
- Check if `<worktree>/deps` directory exists
- Check if `<worktree>/_build` directory exists
- Note if dependencies are installed or missing

### 5. Calculate Statistics

- Total number of worktrees
- Number with services running
- Number with services stopped
- Total ports in use
- Available port offsets (suggest next available)

### 6. Report

Follow the Report section format below.

## Report

After gathering all information, provide a comprehensive report:

```
Git Worktrees Overview

===============================================================

Summary:
   Total Worktrees: <count>
   Running: <count> | Stopped: <count>
   Next Available Port Offset: <offset>

===============================================================

Main Repository
   Location: <project-root>
   Branch: <current-branch>
   Port: 4000
   Status: <RUNNING|STOPPED>

   Actions:
   - Start: iex -S mix phx.server

---------------------------------------------------------------

Worktree: <slug>
   Location: trees/<slug>
   Branch: <branch-name>
   Commit: <commit-hash-short>

   Configuration:
   - Port: <PORT>
   - Database: <database-name>

   Dependencies:
   - deps/: <Installed | Missing>
   - _build/: <Installed | Missing>

   Service Status: <RUNNING (PID: xxxx) | STOPPED>

   Access URL (if running):
   - http://localhost:<PORT>

   Actions:
   - Start: cd trees/<slug> && set -a && source .env && set +a && iex -S mix phx.server
   - Stop: lsof -ti :<PORT> | xargs kill -9
   - Remove: /remove_worktree <branch-name>

---------------------------------------------------------------

[Repeat for each worktree]

===============================================================

Quick Commands:

Create new worktree:
   /create_worktree <branch-name>

Remove worktree:
   /remove_worktree <branch-name>

Start a stopped worktree:
   cd trees/<slug> && set -a && source .env && set +a && iex -S mix phx.server

Stop a running worktree:
   lsof -ti :<PORT> | xargs kill -9

View this list again:
   /list_worktrees

===============================================================
```

If no worktrees exist in trees/:

```
Git Worktrees Overview

===============================================================

Main Repository
   Location: <project-root>
   Branch: <current-branch>
   Port: 4000
   Status: <RUNNING|STOPPED>

===============================================================

No worktrees found in trees/ directory

Create your first worktree:
   /create_worktree <branch-name>

   This will:
   - Create isolated git worktree
   - Configure unique port (4010, 4020, etc.)
   - Set up separate PostgreSQL database
   - Install dependencies

===============================================================
```

If worktrees have configuration issues:

```
Configuration Warnings:

- trees/<slug>: Missing .env file
  Fix: Recreate with /create_worktree <branch-name>

- trees/<slug>: Dependencies not installed
  Fix: cd trees/<slug> && mix deps.get

- trees/<slug>: Missing dev.local.exs (using default database)
  Fix: Create config/dev.local.exs with custom database config
```

## Notes

- Main repository is always shown first (uses port 4000)
- Worktrees are sorted alphabetically by slug
- Service status is checked in real-time
- Port conflicts are detected and highlighted
- Orphaned worktrees (in git but not in trees/) are noted
- PIDs are shown for running processes for easy termination
- All commands are copy-paste ready
