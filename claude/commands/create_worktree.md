---
model: claude-sonnet-4-5-20250929
description: Create a git worktree with isolated configuration for parallel Elixir/Phoenix development
argument-hint: [branch-name]
allowed-tools: Bash, Read, Write, Edit, Glob, Grep
---

# Git Worktree Creation for Elixir

Create a fully functional, isolated clone of the codebase in a separate worktree for parallel Elixir/Phoenix development.

## Arguments

- `$ARGUMENTS` - Branch name for the worktree (required)

## Instructions

### 1. Validate Arguments

Ensure a branch name is provided in `$ARGUMENTS`. If not provided, ask the user for a branch name.

### 2. Generate Worktree Slug

Convert the branch name to a flat directory name by replacing `/` with `-`:

```
WORKTREE_SLUG = $ARGUMENTS with "/" replaced by "-"
```

Examples:
- `feature/new-ui/bla` → `feature-new-ui-bla`
- `fix/auth-bug` → `fix-auth-bug`
- `main` → `main`

### 3. Port Configuration

Base port is defined as:
```
PORT=4000
```

Calculate the worktree port using an offset system:
- List existing worktrees in `trees/` directory
- First worktree gets offset 1 (port 4010)
- Second worktree gets offset 2 (port 4020)
- And so on...

Formula: `WORKTREE_PORT = $PORT + (offset * 10)`

### 4. Pre-Creation Checks

- Verify `trees/` directory exists (create if not)
- Confirm the branch doesn't already have a worktree
- Ensure the calculated port is not in use: `lsof -i :$WORKTREE_PORT`

### 5. Create the Worktree

```bash
# Create trees directory if needed
mkdir -p trees

# Create worktree with slugified name (use existing branch or create new)
git worktree add trees/$WORKTREE_SLUG $ARGUMENTS 2>/dev/null || \
git worktree add -b $ARGUMENTS trees/$WORKTREE_SLUG
```

### 6. Environment Configuration

Create `.env` or `.env.local` in the worktree with the unique port:

```bash
cd trees/$WORKTREE_SLUG

# Copy root .env if it exists
cp ../../.env .env 2>/dev/null || true

# Set the port
echo "PORT=$WORKTREE_PORT" >> .env
```

### 7. Database Configuration

Create `config/dev.local.exs` to override the database for this worktree. The database name should reflect the feature (use the slug):

```elixir
import Config

config :app_name, AppName.Repo,
  database: "app_name_dev_$WORKTREE_SLUG"
```

Note: Replace `app_name` and `AppName` with the actual application name from `mix.exs`.

Then create and migrate the database:

```bash
cd trees/$WORKTREE_SLUG
mix ecto.create
mix ecto.migrate
```

### 8. Install Dependencies

```bash
cd trees/$WORKTREE_SLUG
mix deps.get
mix compile
```

### 9. Report

After successful setup, report:

```
Worktree created successfully!

Branch: $ARGUMENTS
Location: trees/$WORKTREE_SLUG
Port: $WORKTREE_PORT
Database: app_name_dev_$WORKTREE_SLUG

To start the server:
  cd trees/$WORKTREE_SLUG && PORT=$WORKTREE_PORT mix phx.server

To remove worktree:
  git worktree remove trees/$WORKTREE_SLUG
```

## Notes

- Each worktree is completely isolated with its own dependencies and database
- The main repository continues using port $PORT (4000)
- Worktrees use ports 4010, 4020, 4030, etc.
- The `dev.local.exs` file is not committed (should be in `.gitignore`)
