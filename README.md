# Dotfiles

A carefully crafted collection of configuration files for Fish, Tmux, Ghostty, Helix, Zed, mise, and AI coding tools, optimized for cross-platform use.

[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

## Features

- **Cross-platform compatibility**: Works on macOS, Linux, and FreeBSD
- **Theme support**: Custom themes including Finde, Gruvbox Material, Lackluster, Zenbones, and more
- **Modern tools**: Configuration for cutting-edge tools like Ghostty terminal and Zed editor
- **Shell enhancements**: Fish shell with useful abbreviations, functions, and OS-specific configurations
- **Development optimized**: Tmux with sensible defaults, theme switching, and mise-managed tool versions
- **AI coding tools**: Shared agent instructions and skills, plus Pi global configuration

## Components

### Shells

| Component                      | Description                                             |
| ------------------------------ | ------------------------------------------------------- |
| [Fish](https://fishshell.com/) | Shell with good defaults and easy to configure          |
| [Zsh](https://www.zsh.org/)    | Alternative shell with platform-specific configurations |

### Terminals

| Component                                       | Description                              |
| ----------------------------------------------- | ---------------------------------------- |
| [Ghostty](https://github.com/mitchellh/ghostty) | Modern GPU-accelerated terminal emulator |

### Editors & Tools

| Component                              | Description                                                        |
| -------------------------------------- | ------------------------------------------------------------------ |
| [Helix](https://helix-editor.com/)     | A post-modern text editor written in Rust                          |
| [Zed](https://zed.dev/)                | High-performance, multiplayer code editor                          |
| [Tmux](https://github.com/tmux/tmux)   | When editing remote I use Tmux to save sessions and manage windows |
| [Lazygit](https://github.com/jesseduffield/lazygit) | Terminal UI for git commands                          |
| [mise](https://mise.jdx.dev/)        | Runtime and developer tool version manager            |
| [Herdr](https://github.com/ogulcan/herdr) | Lightweight host and command runner configuration |

### AI Coding Tools

| Component                                    | Description                                      |
| -------------------------------------------- | ------------------------------------------------ |
| Shared agents | Tool-agnostic instructions and skills linked to `~/.agents` |
| [Pi](https://github.com/earendil-works/pi-coding-agent) | AI coding agent global settings, models, themes, and extensions |

## AI Coding Tools

This repository includes shared skills and agent instructions for AI coding assistants. Shared Agent Skills and personal coding preferences live in `agents/` and are symlinked into tools that support them.

### Available Skills

| Skill | Description |
| ----- | ----------- |
| `ast-grep` | Syntax-aware code search and rewrite planning |
| `guide` | Interactive guidance to complete a task themselves |
| `smart-commit` | Group uncommitted changes into atomic commits |

### Sharing Architecture

```
agents/           <- Shared AGENTS.md and Agent Skills
     |
     +-> ~/.agents (shared agents)
     +-> ~/.pi/agent/AGENTS.md (Pi global instructions, via make pi)

pi/agent/ <- Pi global settings, models, themes, and extensions
     |
     +-> ~/.pi/agent
```

## Neovim

![Neovim with Telescope](/screenshots/neovim.png)

While not directly configured in this repository, my dotfiles include support for terminal-based editors and tools, complementing my development workflow. The Tmux configuration works particularly well with Neovim for remote editing sessions.

### Clickable Stacktraces (nvim:// URL Handler)

This setup enables clicking file links in your browser (e.g., Phoenix error pages) to open them directly in the correct Neovim instance.

**How it works:**
1. `nvs` starts Neovim with a socket at `/tmp/nvim-$SESSION_NAME`
2. Clicking `nvim://file/...` URLs opens files in that instance
3. Automatically switches tmux session/pane and focuses Ghostty

**Setup:**

1. Run `make bin` to install the handler scripts
2. Install the URL handler app (located at `~/Applications/Nvim URL Handler.app` after first setup)
3. Add to your project's `.mise.toml`:

```toml
[env]
_.source = "~/.local/bin/mise-tmux-session"
PLUG_EDITOR = "nvim://file/__FILE__:__LINE__?tmux-session={{ env.TMUX_SESSION_NAME }}"
```

4. Start Neovim with `nvs` (instead of `nvim`) in your tmux session

## Prerequisites

- Git
- Make
- A package manager (Homebrew on macOS, apt/dnf on Linux)

## Installation

Clone this repository:

```bash
git clone git@github.com:wunki/dotfiles.git
cd dotfiles
```

Use the Makefile to install specific components:

```bash
# Install all configurations
make

# Shells
make fish
make zsh

# Terminals
make ghostty

# Editors & Tools
make helix
make zed
make tmux
make lazygit
make mise
make herdr

# AI Coding Tools
make agents
make pi

# Scripts (nvim URL handler, etc.)
make bin

# Linux system config (root-required, Linux-only; see "Linux System Config")
make linux        # keyd + udev
make keyd
make udev
```

## Shell Setup

### Common Tools (Both Fish and ZSH)

These tools enhance both shells and should be installed first:

```bash
# macOS
brew install eza bat fzf zoxide tree autossh direnv mise gitu

# Debian/Ubuntu
sudo apt install eza bat fzf zoxide tree autossh direnv git gh tmux

# Fedora
sudo dnf install eza bat fzf zoxide tree autossh direnv git gh tmux

# Arch
sudo pacman -S eza bat fzf zoxide tree autossh direnv github-cli tmux
```

### Mise-Managed Global CLIs

Global npm CLIs are tracked with mise's npm backend instead of Node default package files. Node default package files are deprecated, and this keeps tool installs reproducible across machines and Node upgrades.

```bash
mise use -g \
  'npm:hunkdiff@latest' \
  'npm:@earendil-works/pi-coding-agent@latest'
mise install
mise reshim
```

| Tool | Purpose | Command |
| ---- | ------- | ------- |
| `hunkdiff` | Diff hunk formatting/review helper | `hunkdiff` |
| Pi coding agent | AI coding agent CLI | `pi` |

### Fish Setup

1. Install Fisher (plugin manager):
```bash
curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher install jorgebucaran/fisher
```

2. Install plugins:
```bash
fisher install jorgebucaran/autopair.fish
fisher install meaningful-ooo/sponge
fisher install PatrickF1/fzf.fish
fisher install IlanCosman/tide@v6
```

3. Configure Tide prompt:
```bash
tide configure
```

### ZSH Setup

1. Install tools:
```bash
brew install direnv mise gitu
```

2. Clone ZSH plugins:
```bash
mkdir -p ~/.zsh
git clone https://github.com/zsh-users/zsh-autosuggestions.git ~/.zsh/zsh-autosuggestions
git clone https://github.com/zdharma-continuum/fast-syntax-highlighting.git ~/.zsh/fast-syntax-highlighting
git clone https://github.com/hlissner/zsh-autopair.git ~/.zsh/zsh-autopair
git clone https://github.com/sindresorhus/pure.git ~/.zsh/pure
```

## Customization

These dotfiles are organized to be easily customizable:

- OS-specific configurations are isolated in separate files (darwin.fish, linux.fish, freebsd.fish)
- Theme switching is supported in Tmux and Ghostty
- Each tool has its own directory for clean separation of concerns
- Secrets are kept in separate files (secrets.fish) that are not tracked by git

## Keyboard Repeat Rate (GNOME)

On Ubuntu/GNOME (Wayland), key repeat is controlled by two `gsettings` keys
under `org.gnome.desktop.peripherals.keyboard`. These live in GNOME's dconf
database rather than a dotfile, so they need to be set per-machine.

| Key | What it controls | Note |
| --- | ---------------- | ---- |
| `repeat-interval` | Repeat **rate** — ms between repeated characters | Lower = faster. `0` is unsafe (Wayland division-by-zero can break login); `1` is the floor |
| `delay` | Initial **delay** before repeat kicks in | If too short, a normal tap registers a double character. This — not the rate — is what causes accidental doubles |

My values (faster repeat, delay kept high enough to avoid double characters):

```bash
gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 18  # ~55 chars/sec
gsettings set org.gnome.desktop.peripherals.keyboard delay 200
```

Changes apply immediately (no logout needed). To tune:

- **Faster repeat:** lower `repeat-interval` (e.g. `12`)
- **Accidental double characters:** raise `delay` (e.g. `230`)
- **GNOME defaults:** `repeat-interval 30`, `delay 500`

## Linux System Config

Most of this repo is cross-platform user config that symlinks into `~/.config`
or `$HOME`. Anything that is **Linux-only** and installs into `/etc` (so it needs
root) lives under `linux/`, organized by tool. The Makefile encodes each
destination and symlinks the repo file into place, so edits in the repo are live
after a reload.

```
linux/
  keyd/default.conf                   -> /etc/keyd/default.conf
  udev/50-apple-studio-display.rules  -> /etc/udev/rules.d/50-apple-studio-display.rules
```

These targets need `sudo` and are intentionally **not** part of `make all`. Run
them explicitly on Linux:

```bash
make linux   # everything below
make keyd     # capslock/alt remaps + Studio Display brightness keys
make udev     # stable /dev/apple-studio-display node + user access
```

### Apple Studio Display brightness keys

The Studio Display has no `/sys/class/backlight` and no DDC/CI — its brightness
is only reachable over Apple's USB-HID protocol via
[`asdcontrol`](https://github.com/nikosdion/asdcontrol) (built to
`/usr/local/bin/asdcontrol`). The pieces:

- **`bin/asd-brightness`** — `up`/`down`/`get`, adjusts in 6% steps.
- **`linux/udev/50-apple-studio-display.rules`** — exposes a stable
  `/dev/apple-studio-display` symlink (pinned to the display's brightness HID,
  USB interface 07) with `users`-group access, so no `sudo` is needed at runtime.
- **`linux/keyd/default.conf`** — the keyboard's brightness keys arrive as
  `F15`/`F14` (not `XF86MonBrightness`), so keyd's `command()` action runs
  `asd-brightness` directly. This works system-wide regardless of the desktop,
  unlike GNOME custom shortcuts on bare function keys.

Install with `make keyd udev` (or `make linux`). Tune the step size via `STEP`
in `bin/asd-brightness`.

## Troubleshooting

Common issues and solutions:

- **Symlinks not created**: Ensure you have proper permissions in your home directory
- **Fish plugins not working**: Make sure Fisher is installed and run the plugin install commands
- **Theme not applying**: Check that the theme files are properly linked and your terminal supports the colors
- **OS-specific configs not loading**: Verify that the OS detection in the configs matches your system

## License

MIT
