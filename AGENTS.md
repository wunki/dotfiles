# Dotfiles

Repo-specific notes. The shared agent instructions live in `agents/AGENTS.md`.

- `make` symlinks every user-level config into place; `make <tool>` applies one target. The Linux system targets (`make linux`, `make auto-suspend`) need sudo and never run as part of `make`.
- Cendre is the shared theme. The palette files under `bat/`, `eza/`, `fzf/`, `ghostty/`, `helix/`, `pi/`, and `tmux/` are mostly generated upstream; change the wiring around them, not the color values.
- `agents/AGENTS.md` and `agents/skills/` are shared across Claude, Codex, and Pi. Skill directory names are the source of truth; `SKILL.md` is the only file a loader reads.
- Fish is the primary shell and gets the theme and prompt work. Zsh is a daily shell on at least one machine, so keep its feature set in step with Fish rather than trimming it.
- There are no tests. Verify shell changes by sourcing the file in a fresh shell and Makefile changes with `make -n <target>`.
