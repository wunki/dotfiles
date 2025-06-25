# Dotfiles

A carefully crafted collection of configuration files for Fish, Tmux, Ghostty, Helix, Zed and more, optimized for cross-platform use.

[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

## Features

- **Cross-platform compatibility**: Works on macOS, Linux, and FreeBSD
- **Theme support**: Custom themes for terminals and editors including Lackluster (dark) and Zenbones (light)
- **Modern tools**: Configuration for cutting-edge tools like Ghostty terminal and Zed editor
- **Shell enhancements**: Fish shell with useful abbreviations, functions, and OS-specific configurations
- **Development optimized**: Tmux with sensible defaults and theme switching

## Components

| Component                                       | Description                                                        |
| ----------------------------------------------- | ------------------------------------------------------------------ |
| [Fish](https://fishshell.com/)                  | Shell with good defaults and easy to configure                     |
| [Zsh](https://www.zsh.org/)                     | Alternative shell with platform-specific configurations            |
| [Tmux](https://github.com/tmux/tmux)            | When editing remote I use Tmux to save sessions and manage windows |
| [Ghostty](https://github.com/mitchellh/ghostty) | Modern GPU-accelerated terminal emulator                           |
| [Helix](https://helix-editor.com/)              | A post-modern text editor written in Rust                          |
| [Zed](https://zed.dev/)                         | High-performance, multiplayer code editor                          |

## Neovim

![Neovim with Telescope](/screenshots/neovim.png)

While not directly configured in this repository, my dotfiles include support for terminal-based editors and tools, complementing my development workflow. The Tmux configuration works particularly well with Neovim for remote editing sessions.

## Prerequisites

- Git
- Make
- For Fish configuration: [Fisher](https://github.com/jorgebucaran/fisher) package manager
- Terminal emulator compatible with your platform (Ghostty, iTerm2, etc.)

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

# Install specific components
make fish
make tmux
make helix
make ghostty
make zed
make zsh
```

After installing Fish configuration, you may want to install the recommended plugins:

```bash
# Install Fish plugins
fisher install jorgebucaran/hydro
fisher install jorgebucaran/autopair.fish
fisher install jethrokuan/z
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

