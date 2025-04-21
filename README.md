# Dotfiles

A carefully crafted collection of configuration files for Fish, Tmux, Ghostty, Helix, Zed and more, optimized for cross-platform use.

![Terminal Screenshot](/screenshots/fullscreen.png)

## Features

- **Cross-platform compatibility**: Works on macOS, Linux, and FreeBSD
- **Theme support**: Custom themes for terminals and editors including Lackluster (dark) and Zenbones (light)
- **Modern tools**: Configuration for cutting-edge tools like Ghostty terminal and Zed editor
- **Shell enhancements**: Fish shell with useful abbreviations, functions, and OS-specific configurations
- **Development optimized**: Tmux with sensible defaults and theme switching

## Components

| Component | Description |
|-----------|-------------|
| [Fish](https://fishshell.com/) | Shell with good defaults and easy to configure |
| [Tmux](https://github.com/tmux/tmux) | When editing remote I use Tmux to save sessions and manage windows |
| [Ghostty](https://github.com/mitchellh/ghostty) | Modern GPU-accelerated terminal emulator |
| [Helix](https://helix-editor.com/) | A post-modern text editor written in Rust |
| [Zed](https://zed.dev/) | High-performance, multiplayer code editor |

## Neovim

![Neovim with Telescope](/screenshots/neovim.png)

My dotfiles include support for terminal-based editors and tools, complementing my development workflow.

## Installation

Clone this repository:

```bash
git clone https://github.com/your-username/dotfiles.git
cd dotfiles
```

Use the Makefile to install specific components:

```bash
# Install all configurations
make

# Install specific components
make fish
make tmux
make helix
make ghostty
make zed
make zsh
```

## Customization

These dotfiles are organized to be easily customizable:

- OS-specific configurations are isolated in separate files
- Theme switching is supported in Tmux and Ghostty
- Each tool has its own directory for clean separation of concerns

## License

MIT