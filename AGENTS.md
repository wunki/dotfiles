# Dotfiles

Repo-specific notes.

- `make` symlinks every user-level config into place; `make <tool>` applies one target. The Linux system targets (`make linux`, `make auto-suspend`) need sudo and never run as part of `make`.
- Cendre is the shared theme. The palette files under `bat/`, `eza/`, `fzf/`, `ghostty/`, `helix/`, `pi/`, and `tmux/` are mostly generated upstream; change the wiring around them, not the color values.
- Agent instructions, skills, and Pi config (extensions, settings, models) live in `~/Code/petar-skills`, which links itself; change them there, not here. This repo only wires the Cendre theme into Codex (`make codex`) and Pi (`make pi`).
- Fish is the primary shell and gets the theme and prompt work. Zsh is a daily shell on at least one machine, so keep its feature set in step with Fish rather than trimming it.
- Trunk-based: commit to `main` and push. Don't open PRs.
- There are no tests. Verify shell changes by sourcing the file in a fresh shell and Makefile changes with `make -n <target>`.
