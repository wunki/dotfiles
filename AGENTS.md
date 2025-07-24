# AGENTS.md - Dotfiles Repository Guide

## Build, Lint, and Test Commands

- **Apply dotfiles**: `make` (creates symlinks to ~/.config and ~/)
- **Install specific tool**: `make fish`, `make tmux`, `make helix`, `make ghostty`, `make zed`, `make zsh`
- **No linting/testing**: This is a configuration repository with no automated tests

## Code Style Guidelines

### General
- This repository contains configuration files for development tools (fish, zsh, helix, tmux, ghostty, zed)
- Follow existing conventions within each configuration file type
- Use comments to explain complex logic or non-obvious configurations

### Shell Scripts (Fish/Zsh)
- Use lowercase with underscores for variables: `my_variable`
- Comment complex functions and abbreviations
- OS-specific code goes in separate files (darwin.fish, linux.fish, freebsd.fish)
- Keep secrets in separate untracked files (secrets.fish)
- Use `set -x` for exports in Fish, `export` in Zsh

### Configuration Files
- Maintain consistent indentation (spaces preferred)
- Group related settings together with comments
- Use descriptive names for custom themes and snippets
- Follow each tool's native configuration format (TOML for Helix, JSON for Zed, etc.)
