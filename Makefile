# Variables
DOTFILES	:= $(PWD)
CONFIG_DIR	:= ${HOME}/.config
UNAME		:= $(shell uname -s)

# List all application targets here
APP_TARGETS := fish zsh helix ghostty rio zed tmux opencode bin amp claude codex lazygit

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
	@printf "Recommended tools (install via brew or package manager):\n"
	@printf "\tbrew install eza        # Modern ls replacement\n"
	@printf "\tbrew install bat        # Modern cat replacement\n"
	@printf "\tbrew install fzf        # Fuzzy finder\n\n"
	@printf "Fisher plugins (run after installing fisher):\n"
	@printf "\tfisher install jorgebucaran/autopair.fish\n"
	@printf "\tfisher install jethrokuan/z\n"
	@printf "\tfisher install PatrickF1/fzf.fish\n"
	@printf "\tfisher install IlanCosman/tide@v6\n\n"

zsh:
	@echo "Linking zsh configuration..."
	@ln -fns $(DOTFILES)/zsh/zprofile $(HOME)/.zprofile
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
	@ln -fns $(DOTFILES)/tmux/tmux-flexoki-light-theme.conf $(HOME)/.tmux-flexoki-light-theme.conf
	@echo "tmux linked."

opencode: ensure-config-dir
	@echo "Linking opencode configuration..."
	@ln -fns $(DOTFILES)/opencode $(CONFIG_DIR)/opencode
	@mkdir -p $(HOME)/.opencode
	@ln -fns $(DOTFILES)/claude/skills $(HOME)/.opencode/skill
	@echo "OpenCode linked (skills shared with Claude Code)."

amp: ensure-config-dir
	@echo "Linking amp configuration..."
	@mkdir -p $(CONFIG_DIR)/amp
	@ln -fns $(DOTFILES)/amp/settings.json $(CONFIG_DIR)/amp/settings.json
	@ln -fns $(DOTFILES)/claude/commands $(CONFIG_DIR)/amp/commands
	@echo "Amp linked."

claude:
	@echo "Linking claude configuration..."
	@mkdir -p $(HOME)/.claude
	@ln -fns $(DOTFILES)/claude/skills $(HOME)/.claude/skills
	@ln -fns $(DOTFILES)/claude/commands $(HOME)/.claude/commands
	@echo "Claude linked."

codex:
	@echo "Linking codex configuration..."
	@mkdir -p $(HOME)/.codex/skills
	@ln -fns $(DOTFILES)/codex/config.toml $(HOME)/.codex/config.toml
	@for skill in $(DOTFILES)/claude/skills/*/; do \
		name=$$(basename "$$skill"); \
		if [ "$$name" != "skill-creator" ]; then \
			ln -fns "$$skill" "$(HOME)/.codex/skills/$$name"; \
			echo "  Linked skill: $$name"; \
		fi \
	done
	@echo "Codex linked (skills shared with Claude Code)."

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

# --- Utility Targets ---

# Print the value of any make variable (e.g., make print-DOTFILES)
print-%:
	@echo '$* = $($*)'
