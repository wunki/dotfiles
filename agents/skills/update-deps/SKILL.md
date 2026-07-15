---
name: update-deps
description: >
  Upgrades Elixir Mix/Hex dependencies safely, covering patch/minor bumps, major-version
  migrations with changelog research, and post-update verification. Use when the user asks
  to update deps, bump packages, upgrade a specific Hex package, or run mix hex.outdated.
  Don't use for: adding a new dependency, removing a dependency, resolving a dependency
  conflict or lockfile error without upgrading, or updating npm/pip/cargo/other non-Elixir
  package managers.
---

# Update Elixir Dependencies

Use this workflow for Elixir projects that use Mix + Hex.

## Workflow

### 1. Baseline And Scope

1. Run `git status --short`.
   - If there are uncommitted changes: report them and ask via `AskUserQuestion` (single-select):
     - "Stash changes" — stash and continue
     - "Abort" — stop the upgrade
     - "Proceed anyway" — continue with dirty tree
   Do not silently proceed with a dirty tree.
2. Verify Hex is available: `mix hex.info` (no args). If Hex is not installed, run `mix local.hex --force` first.
3. Identify outdated dependencies: `mix hex.outdated`.
4. Split findings into:
   - Patch/minor upgrades (usually non-breaking).
   - Major upgrades (potentially breaking).
5. For umbrella projects (root `mix.exs` contains `apps_path`): note that version constraints may need updating in each `apps/<app>/mix.exs`. `mix deps.update` from the root works for all apps.

### 2. Apply Patch/Minor Upgrades First

1. Update dependencies that do not require major version constraint changes.
2. Prefer targeted updates for easier debugging:
   - `mix deps.update <dep_name>`
3. If many safe updates exist and risk is low, `mix deps.update --all` is acceptable.
4. The lockfile (`mix.lock`) is updated automatically by `mix deps.update`. Commit it together with any `mix.exs` changes.
5. Run verification (Section 4) before touching major upgrades.

### 3. Handle Major Upgrades One Dependency At A Time

For each dependency with a major bump:

1. Update the version constraint in `mix.exs`. Use `~> X.0` (e.g., `~> 2.0`) for most libraries. Only use `>= X.0.0 and < Y.0.0` if the library explicitly recommends it in its upgrade guide.
2. Run `mix deps.update <dep_name>` to apply the new constraint.
3. Gather migration notes from primary sources, in this order:
   - `mix hex.info <dep_name>` to find package/repo links.
   - `https://hexdocs.pm/<dep_name>` (look for changelog, migration, or upgrade sections).
   - Repository `CHANGELOG*`, release notes, and upgrade guides.
4. Extract only changes relevant to the jump from current to target major version.
5. Implement required code/config updates.
6. Run verification (Section 4) before moving to the next major dependency.

If a major upgrade fails verification and cannot be resolved quickly:
1. Revert only that dependency change (`git checkout mix.exs mix.lock`).
2. Continue with other upgrades.
3. Report the blocker with exact failing errors and required follow-up.

### 4. Verification

Run these commands after each update batch and after each major dependency:

```sh
mix compile --warnings-as-errors
mix format --check-formatted
mix test
```

If `mix compile --warnings-as-errors` fails due to pre-existing warnings unrelated to the upgrade, note them and rerun without `--warnings-as-errors` (`mix compile`) to confirm the upgrade itself is clean. Do not fix pre-existing warnings unless asked.

If the project defines additional CI checks, run the same checks used in `.github/workflows`.

Failure handling:
- Compile warnings/errors introduced by the upgrade: fix code or config issues.
- Format check failures: run `mix format`.
- Test failures: fix behavior regressions before proceeding.

### 5. Completion

1. Run `mix hex.outdated` again and confirm the remaining items (if any) are intentional.
2. Summarize:
   - Dependencies updated and final versions.
   - Major migrations performed and code changes required.
   - Any intentionally deferred upgrades and why.
