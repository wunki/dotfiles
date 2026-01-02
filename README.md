# Dotfiles

A carefully crafted collection of configuration files for Fish, Tmux, Ghostty, Helix, Zed and more, optimized for cross-platform use.

[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

## Features

- **Cross-platform compatibility**: Works on macOS, Linux, and FreeBSD
- **Theme support**: Custom themes including Finde, Gruvbox Material, Lackluster, Zenbones, and more
- **Modern tools**: Configuration for cutting-edge tools like Ghostty terminal and Zed editor
- **Shell enhancements**: Fish shell with useful abbreviations, functions, and OS-specific configurations
- **Development optimized**: Tmux with sensible defaults and theme switching
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

### AI Coding Tools

| Component                                    | Description                                      |
| -------------------------------------------- | ------------------------------------------------ |
| [Claude Code](https://claude.ai/)            | Anthropic's CLI for Claude (skills and commands) |
| [OpenCode](https://opencode.ai/)             | AI coding assistant (shares skills with Claude)  |
| [Amp](https://amp.dev/)                      | AI-powered editor (shares commands with Claude)  |
| [Codex](https://github.com/openai/codex-cli) | OpenAI's CLI (shares skills with Claude)         |

## AI Coding Tools

This repository includes a shared ecosystem of skills and commands for AI coding assistants. The skills are defined once in `claude/skills/` and symlinked to other tools.

### Available Skills

| Skill | Description |
| ----- | ----------- |
| `dev-browser` | Browser automation with persistent page state |
| `frontend-design` | Create distinctive, production-grade frontend interfaces |
| `ship` | Create a concrete plan for finishing a project |
| `teach` | Guide the user to complete a task themselves |
| `solveit` | Generate structured learning guides using the Polya method |
| `beads` | Track complex, multi-session work with dependency graphs |
| `skill-creator` | Guide for creating new skills |

### Sharing Architecture

```
claude/skills/     <- Primary skill definitions
     |
     +-> ~/.claude/skills (Claude Code)
     +-> ~/.opencode/skill (OpenCode)
     +-> ~/.codex/skills (Codex)

claude/commands/   <- Shared commands
     |
     +-> ~/.claude/commands (Claude Code)
     +-> ~/.config/amp/commands (Amp)
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

# AI Coding Tools
make claude
make opencode
make amp
make codex

# Scripts (nvim URL handler, etc.)
make bin
```

## Shell Setup

### Common Tools (Both Fish and ZSH)

These tools enhance both shells and should be installed first:

```bash
brew install eza         # Modern ls replacement (aliased to ls)
brew install bat         # Modern cat replacement (aliased to cat)
brew install fzf         # Fuzzy finder
```

### Fish Setup

1. Install Fisher (plugin manager):
```bash
curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher install jorgebucaran/fisher
```

2. Install plugins:
```bash
fisher install jorgebucaran/autopair.fish
fisher install jethrokuan/z
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
brew install zoxide      # Directory jumping (use 'z' command)
brew install direnv      # Per-directory environment variables
brew install mise        # Version manager for dev tools
brew install gitu        # Terminal UI for git (aliased to 'gu')
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

