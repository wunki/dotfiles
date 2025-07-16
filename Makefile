# Variables
DOTFILES	:= $(PWD)
CONFIG_DIR	:= ${HOME}/.config
UNAME		:= $(shell uname -s)

# List all application targets here
APP_TARGETS := fish zsh helix ghostty zed tmux

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
	@printf "Fish linked. Run fisher commands manually if needed:\n"
	@printf "\t'fisher install jorgebucaran/hydro'\n"
	@printf "\t'fisher install jorgebucaran/autopair.fish'\n"
	@printf "\t'fisher install jethrokuan/z'\n\n"
	@printf "\t'fisher install PatrickF1/fzf.fish'\n\n"

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

helix: ensure-config-dir
	@echo "Linking helix configuration..."
	@ln -fns $(DOTFILES)/helix $(CONFIG_DIR)/helix
	@echo "Helix linked."

ghostty: ensure-config-dir
	@echo "Linking ghostty configuration..."
	@ln -fns $(DOTFILES)/ghostty $(CONFIG_DIR)/ghostty
	@echo "Ghostty linked."

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
	@echo "tmux linked."

# --- Utility Targets ---

# Print the value of any make variable (e.g., make print-DOTFILES)
print-%:
	@echo '$* = $($*)'
