# Dotfiles

A carefully crafted collection of configuration files for Fish, Tmux, Ghostty, Helix, Zed, mise and more, optimized for cross-platform use.

[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

## Features

- **Cross-platform compatibility**: Works on macOS, Linux, and FreeBSD
- **Theme support**: Custom themes including Finde, Gruvbox Material, Lackluster, Zenbones, and more
- **Modern tools**: Configuration for cutting-edge tools like Ghostty terminal and Zed editor
- **Shell enhancements**: Fish shell with useful abbreviations, functions, and OS-specific configurations
- **Development optimized**: Tmux with sensible defaults, theme switching, and mise-managed tool versions
- **AI coding tools**: Shared skills and commands for Claude Code, OpenCode, Amp, and Codex

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
| [Rio](https://raphamorim.io/rio/)               | Hardware-accelerated terminal            |

### Editors & Tools

| Component                              | Description                                                        |
| -------------------------------------- | ------------------------------------------------------------------ |
| [Helix](https://helix-editor.com/)     | A post-modern text editor written in Rust                          |
| [Zed](https://zed.dev/)                | High-performance, multiplayer code editor                          |
| [Tmux](https://github.com/tmux/tmux)   | When editing remote I use Tmux to save sessions and manage windows |
| [Lazygit](https://github.com/jesseduffield/lazygit) | Terminal UI for git commands                          |
| [mise](https://mise.jdx.dev/)        | Runtime and developer tool version manager            |

### AI Coding Tools

| Component                                    | Description                                      |
| -------------------------------------------- | ------------------------------------------------ |
| [Claude Code](https://claude.ai/)            | Anthropic's CLI for Claude (skills and commands) |
| [OpenCode](https://opencode.ai/)             | AI coding assistant (shares skills with Claude)  |
| [Amp](https://amp.dev/)                      | AI-powered editor (shares commands with Claude)  |
| [Codex](https://github.com/openai/codex-cli) | OpenAI's CLI (shares skills with Claude)         |
| [Pi](https://github.com/earendil-works/pi-coding-agent) | AI coding agent (global instructions and extensions) |

## AI Coding Tools

This repository includes a shared ecosystem of skills, commands, and agent instructions for AI coding assistants. Shared Agent Skills and personal coding preferences live in `agents/` and are symlinked into tools that support them.

### Available Skills

| Skill | Description |
| ----- | ----------- |
| `dev-browser` | Browser automation with persistent page state |
| `frontend-design` | Create distinctive, production-grade frontend interfaces |
| `ship` | Create a concrete plan for finishing a project |
| `guide` | Interactive guidance to complete a task themselves |
| `solveit` | Generate structured learning guides using the Polya method |
| `beads` | Track complex, multi-session work with dependency graphs |
| `skill-creator` | Guide for creating new skills |

### Sharing Architecture

```
agents/           <- Shared AGENTS.md and Agent Skills
     |
     +-> ~/.agents (shared agents)
     +-> ~/.pi/agent/AGENTS.md (Pi global instructions)

pi/agent/extensions/ <- Pi global extensions
     |
     +-> ~/.pi/agent/extensions
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
git clone https://github.com/petar/dotfiles.git
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
make rio

# Editors & Tools
make helix
make zed
make tmux
make lazygit
make mise

# AI Coding Tools
make agents
make pi

# Scripts (nvim URL handler, etc.)
make bin
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

## Troubleshooting

Common issues and solutions:

- **Symlinks not created**: Ensure you have proper permissions in your home directory
- **Fish plugins not working**: Make sure Fisher is installed and run the plugin install commands
- **Theme not applying**: Check that the theme files are properly linked and your terminal supports the colors
- **OS-specific configs not loading**: Verify that the OS detection in the configs matches your system

## License

MIT
