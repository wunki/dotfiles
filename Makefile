# Variables
DOTFILES	:= $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
CONFIG_DIR	:= ${HOME}/.config
UNAME		:= $(shell uname -s)

# List all application targets here
APP_TARGETS := fish zsh helix ghostty rio zed tmux bin lazygit mise agents pi

# Define the default target 'all' to depend on all application targets
.PHONY: all
all: $(APP_TARGETS)
	@echo "All specified dotfiles linked."

# Declare all command targets as .PHONY
.PHONY: $(APP_TARGETS) print-% ensure-config-dir

# --- Application Targets ---

# Target to ensure the ~/.config directory exists
.PHONY: ensure-config-dir
ensure-config-dir:
	@echo "Ensuring $(CONFIG_DIR) exists..."
	@mkdir -p $(CONFIG_DIR)

fish: ensure-config-dir
	@echo "Linking fish configuration..."
	@ln -fns $(DOTFILES)/fish $(CONFIG_DIR)/fish
	@printf "Fish linked.\n\n"
ifeq ($(UNAME),Darwin)
	@printf "Recommended tools:\n"
	@printf "\tbrew install eza bat fzf zoxide tree autossh direnv mise gitu\n\n"
else
	@printf "Recommended tools:\n"
	@printf "\tDebian/Ubuntu: sudo apt install eza bat fzf zoxide tree autossh direnv git gh tmux\n"
	@printf "\tFedora:        sudo dnf install eza bat fzf zoxide tree autossh direnv git gh tmux\n"
	@printf "\tArch:          sudo pacman -S eza bat fzf zoxide tree autossh direnv github-cli tmux\n"
	@printf "\tInstall separately as needed: mise, gitu, bun, pnpm, opencode, lua-language-server\n\n"
endif
	@printf "Fisher plugins (run after installing fisher):\n"
	@printf "\tfisher install jorgebucaran/autopair.fish\n"
	@printf "\tfisher install meaningful-ooo/sponge\n"
	@printf "\tfisher install PatrickF1/fzf.fish\n"
	@printf "\tfisher install IlanCosman/tide@v6\n\n"

zsh:
	@echo "Linking zsh configuration..."
	@ln -fns $(DOTFILES)/zsh/zshrc $(HOME)/.zshrc
ifeq ($(UNAME),Darwin)
	@ln -fns $(DOTFILES)/zsh/zshrc.mac $(HOME)/.zshrc.mac
	@echo "ZSH linked (including macOS specific file)."
else
	@echo "ZSH linked."
endif
	@printf "Recommended tools (install via brew or package manager):\n"
	@printf "\tbrew install zoxide     # Directory jumping\n"
	@printf "\tbrew install eza        # Modern ls replacement\n"
	@printf "\tbrew install bat        # Modern cat replacement\n"
	@printf "\tbrew install direnv     # Per-directory environment\n"
	@printf "\tbrew install mise       # Version manager\n"
	@printf "\tbrew install gitu       # Terminal UI for git\n\n"
	@printf "ZSH plugins (clone manually):\n"
	@printf "\tgit clone https://github.com/zsh-users/zsh-autosuggestions.git ~/.zsh/zsh-autosuggestions\n"
	@printf "\tgit clone https://github.com/zdharma-continuum/fast-syntax-highlighting.git ~/.zsh/fast-syntax-highlighting\n"
	@printf "\tgit clone https://github.com/hlissner/zsh-autopair.git ~/.zsh/zsh-autopair\n"
	@printf "\tgit clone https://github.com/sindresorhus/pure.git ~/.zsh/pure\n\n"

helix: ensure-config-dir
	@echo "Linking helix configuration..."
	@ln -fns $(DOTFILES)/helix $(CONFIG_DIR)/helix
	@echo "Helix linked."

ghostty: ensure-config-dir
	@echo "Linking ghostty configuration..."
	@ln -fns $(DOTFILES)/ghostty $(CONFIG_DIR)/ghostty
	@echo "Ghostty linked."

rio: ensure-config-dir
	@echo "Linking rio configuration..."
	@ln -fns $(DOTFILES)/rio $(CONFIG_DIR)/rio
	@echo "Rio linked."

zed: ensure-config-dir
	@echo "Linking zed configuration..."
	@ln -fns $(DOTFILES)/zed $(CONFIG_DIR)/zed
	@echo "Zed linked."

tmux:
	@echo "Linking tmux configuration..."
	@ln -fns $(DOTFILES)/tmux/tmux.conf $(HOME)/.tmux.conf
	@ln -fns $(DOTFILES)/tmux/tmux-lackluster-theme.conf $(HOME)/.tmux-lackluster-theme.conf
	@ln -fns $(DOTFILES)/tmux/tmux-lackluster-hack-theme.conf $(HOME)/.tmux-lackluster-hack-theme.conf
	@ln -fns $(DOTFILES)/tmux/tmux-gruvbox-material-theme.conf $(HOME)/.tmux-gruvbox-material-theme.conf
	@ln -fns $(DOTFILES)/tmux/tmux-zenbones-theme.conf $(HOME)/.tmux-zenbones-theme.conf
	@ln -fns $(DOTFILES)/tmux/tmux-github-dark-theme.conf $(HOME)/.tmux-github-dark-theme.conf
	@ln -fns $(DOTFILES)/tmux/tmux-zed-dark-theme.conf $(HOME)/.tmux-zed-dark-theme.conf
	@ln -fns $(DOTFILES)/tmux/tmux-ayu-dark-theme.conf $(HOME)/.tmux-ayu-dark-theme.conf
	@ln -fns $(DOTFILES)/tmux/tmux-melange-light-theme.conf $(HOME)/.tmux-melange-light-theme.conf
	@ln -fns $(DOTFILES)/tmux/tmux-rose-pine-dawn-theme.conf $(HOME)/.tmux-rose-pine-dawn-theme.conf
	@ln -fns $(DOTFILES)/tmux/tmux-flexoki-light-theme.conf $(HOME)/.tmux-flexoki-light-theme.conf
	@echo "tmux linked."


lazygit:
	@echo "Linking lazygit configuration..."
ifeq ($(UNAME),Darwin)
	@mkdir -p $(HOME)/Library/Application\ Support
	@ln -fns $(DOTFILES)/lazygit $(HOME)/Library/Application\ Support/lazygit
else
	@mkdir -p $(CONFIG_DIR)
	@ln -fns $(DOTFILES)/lazygit $(CONFIG_DIR)/lazygit
endif
	@echo "Lazygit linked."

mise: ensure-config-dir
	@echo "Linking mise configuration..."
	@mkdir -p $(CONFIG_DIR)/mise
	@ln -fns $(DOTFILES)/mise/config.toml $(CONFIG_DIR)/mise/config.toml
	@echo "Mise linked."

bin:
	@echo "Linking bin scripts to ~/.local/bin..."
	@mkdir -p $(HOME)/.local/bin
	@for f in $(DOTFILES)/bin/*; do \
		if [ -x "$$f" ] && [ -f "$$f" ]; then \
			name=$$(basename "$$f"); \
			name=$${name%.*}; \
			ln -fns "$$f" "$(HOME)/.local/bin/$$name"; \
			echo "  Linked $$name"; \
		fi \
	done
	@echo "Bin scripts linked."

agents:
	@echo "Linking shared agent instructions and skills..."
	@if [ -e $(HOME)/.agents ] && [ ! -L $(HOME)/.agents ]; then \
		backup=$(HOME)/.agents.bak.$$(date +%Y%m%d%H%M%S); \
		mv $(HOME)/.agents $$backup; \
		echo "Backed up existing ~/.agents to $$backup"; \
	fi
	@ln -fns $(DOTFILES)/agents $(HOME)/.agents
	@echo "Shared agents linked."

pi: agents
	@echo "Linking Pi global configuration..."
	@mkdir -p $(HOME)/.pi/agent/extensions
	@ln -fns $(HOME)/.agents/AGENTS.md $(HOME)/.pi/agent/AGENTS.md
	@ln -fns $(DOTFILES)/pi/agent/extensions/tmux.ts $(HOME)/.pi/agent/extensions/tmux.ts
	@echo "Pi linked."

# --- Utility Targets ---

# Print the value of any make variable (e.g., make print-DOTFILES)
print-%:
	@echo '$* = $($*)'
