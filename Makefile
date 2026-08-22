# Variables
DOTFILES	:= $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
CONFIG_DIR	:= ${HOME}/.config
UNAME		:= $(shell uname -s)

# List all application targets here
APP_TARGETS := fish zsh bat helix ghostty zed sublime tmux herdr bin lazygit mise agents claude codex pi

# Define the default target 'all' to depend on all application targets
.PHONY: all
all: $(APP_TARGETS)
	@echo "All specified dotfiles linked."

# Declare all command targets as .PHONY
.PHONY: $(APP_TARGETS) print-% ensure-config-dir setup-clojure-lsp setup-neil

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
	@printf "\tfisher install IlanCosman/tide@v6\n"
	@printf "\tThen run tide configure and choose Lean / 16 colors.\n\n"

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

bat: ensure-config-dir
	@echo "Linking bat configuration..."
	@ln -fns $(DOTFILES)/bat $(CONFIG_DIR)/bat
	@if command -v bat >/dev/null 2>&1; then \
		bat cache --build; \
		echo "Bat linked and theme cache rebuilt."; \
	else \
		echo "Bat linked; install bat to build the theme cache."; \
	fi

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

sublime:
	@echo "Installing Sublime Text configuration..."
ifeq ($(UNAME),Darwin)
	@user_dir="$(HOME)/Library/Application Support/Sublime Text/Packages/User"; \
		mkdir -p "$$user_dir"; \
		for file in \
			"Preferences.sublime-settings" \
			"Package Control.sublime-settings" \
			"LanguageServers.sublime-settings" \
			"ayu-dark.sublime-theme" \
			"Default (OSX).sublime-keymap"; do \
			destination="$$user_dir/$$file"; \
			if [ -L "$$destination" ]; then unlink "$$destination"; fi; \
			install -m 0644 "$(DOTFILES)/sublime/$$file" "$$destination"; \
		done
	@mkdir -p $(HOME)/.local/bin
	@ln -fns "/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl" $(HOME)/.local/bin/subl
else
	@user_dir="$(CONFIG_DIR)/sublime-text/Packages/User"; \
		mkdir -p "$$user_dir"; \
		for file in \
			"Preferences.sublime-settings" \
			"Package Control.sublime-settings" \
			"LanguageServers.sublime-settings" \
			"ayu-dark.sublime-theme"; do \
			destination="$$user_dir/$$file"; \
			if [ -L "$$destination" ]; then unlink "$$destination"; fi; \
			install -m 0644 "$(DOTFILES)/sublime/$$file" "$$destination"; \
		done
endif
	@echo "Sublime Text configuration installed."

tmux: ensure-config-dir
	@echo "Linking tmux configuration..."
	@ln -fns $(DOTFILES)/tmux $(CONFIG_DIR)/tmux
	@ln -fns $(CONFIG_DIR)/tmux/tmux.conf $(HOME)/.tmux.conf
	@echo "tmux linked."

herdr: ensure-config-dir
	@echo "Linking Herdr configuration..."
	@mkdir -p $(CONFIG_DIR)/herdr
	@ln -fns $(DOTFILES)/herdr/config.toml $(CONFIG_DIR)/herdr/config.toml
	@echo "Herdr linked."

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
	@for link in $(HOME)/.local/bin/*; do \
		if [ -L "$$link" ]; then \
			target=$$(readlink "$$link"); \
			case "$$target" in \
				$(DOTFILES)/bin/*) \
					if [ ! -e "$$target" ]; then \
						rm "$$link"; \
						echo "  Removed stale $$(basename "$$link")"; \
					fi; \
					;; \
			esac; \
		fi; \
	done
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

claude: agents
	@echo "Linking shared skills for Claude..."
	@mkdir -p $(HOME)/.claude
	@if [ -e $(HOME)/.claude/skills ] && [ ! -L $(HOME)/.claude/skills ]; then \
		backup=$(HOME)/.claude/skills.bak.$$(date +%Y%m%d%H%M%S); \
		mv $(HOME)/.claude/skills $$backup; \
		echo "Backed up existing Claude skills to $$backup"; \
	fi
	@ln -fns $(DOTFILES)/agents/skills $(HOME)/.claude/skills
	@echo "Claude skills linked."

codex: agents
	@echo "Linking Codex global instructions..."
	@mkdir -p $(HOME)/.codex
	@if [ -e $(HOME)/.codex/AGENTS.md ] && [ ! -L $(HOME)/.codex/AGENTS.md ]; then \
		backup=$(HOME)/.codex/AGENTS.md.bak.$$(date +%Y%m%d%H%M%S); \
		mv $(HOME)/.codex/AGENTS.md $$backup; \
		echo "Backed up existing ~/.codex/AGENTS.md to $$backup"; \
	fi
	@ln -fns $(DOTFILES)/agents/AGENTS.md $(HOME)/.codex/AGENTS.md
	@echo "Codex linked."

pi:
	@echo "Linking Pi global configuration..."
	@mkdir -p $(HOME)/.pi/agent
	@ln -fns $(DOTFILES)/agents/AGENTS.md $(HOME)/.pi/agent/AGENTS.md
	@if [ -e $(HOME)/.pi/agent/extensions ] && [ ! -L $(HOME)/.pi/agent/extensions ]; then \
		backup=$(HOME)/.pi/agent/extensions.bak.$$(date +%Y%m%d%H%M%S); \
		mv $(HOME)/.pi/agent/extensions $$backup; \
		echo "Backed up existing Pi extensions to $$backup"; \
	fi
	@ln -fns $(DOTFILES)/pi/agent/extensions $(HOME)/.pi/agent/extensions
	@if [ -e $(HOME)/.pi/agent/themes ] && [ ! -L $(HOME)/.pi/agent/themes ]; then \
		backup=$(HOME)/.pi/agent/themes.bak.$$(date +%Y%m%d%H%M%S); \
		mv $(HOME)/.pi/agent/themes $$backup; \
		echo "Backed up existing Pi themes to $$backup"; \
	fi
	@ln -fns $(DOTFILES)/pi/agent/themes $(HOME)/.pi/agent/themes
	@ln -fns $(DOTFILES)/pi/agent/settings.json $(HOME)/.pi/agent/settings.json
	@ln -fns $(DOTFILES)/pi/agent/models.json $(HOME)/.pi/agent/models.json
	@echo "Pi linked."

# --- Linux System Targets ---
#
# These install system configuration and require root, so they are Linux-only
# and NOT part of `make all`. Source files live under linux/<tool>/; the
# Makefile encodes each destination.

# Install the shared Linux system configuration.
.PHONY: linux
linux: keyd udev
	@echo "Linux system configuration linked."

# auto-suspend: suspend this desktop after 30 minutes without an active session.
# Install root-owned copies because systemd runs the monitor as root.
.PHONY: auto-suspend
auto-suspend:
ifeq ($(UNAME),Linux)
	@command -v shellcheck >/dev/null || { echo "shellcheck is required." >&2; exit 1; }
	@command -v python3 >/dev/null || { echo "python3 is required." >&2; exit 1; }
	@command -v runuser >/dev/null || { echo "runuser is required." >&2; exit 1; }
	@shellcheck $(DOTFILES)/linux/systemd/auto-suspend-monitor/auto-suspend-monitor
	@echo "Installing automatic suspend monitor (requires sudo)..."
	@sudo install -d -o root -g root -m 0755 /usr/local/sbin /etc/systemd/system
	@sudo install -o root -g root -m 0755 $(DOTFILES)/linux/systemd/auto-suspend-monitor/auto-suspend-monitor /usr/local/sbin/auto-suspend-monitor
	@sudo install -o root -g root -m 0644 $(DOTFILES)/linux/systemd/auto-suspend-monitor/auto-suspend-monitor.service /etc/systemd/system/auto-suspend-monitor.service
	@sudo install -o root -g root -m 0644 $(DOTFILES)/linux/systemd/auto-suspend-monitor/auto-suspend-monitor.timer /etc/systemd/system/auto-suspend-monitor.timer
	@sudo systemctl daemon-reload
	@sudo systemctl enable auto-suspend-monitor.timer
	@sudo systemctl restart auto-suspend-monitor.timer
	@echo "Automatic suspend monitor installed and enabled."
else
	@echo "auto-suspend target is Linux-only; skipping on $(UNAME)."
endif

# keyd: capslock/alt remaps + Apple Studio Display brightness keys
# (F15/F14 -> asd-brightness). Depends on `bin` so the script is linked first.
.PHONY: keyd
keyd: bin
ifeq ($(UNAME),Linux)
	@echo "Linking keyd configuration to /etc/keyd (requires sudo)..."
	@sudo ln -fns $(DOTFILES)/linux/keyd/default.conf /etc/keyd/default.conf
	@sudo sh -c 'keyd reload 2>/dev/null || keyd.rvaiya reload 2>/dev/null || systemctl restart keyd'
	@echo "keyd linked and reloaded."
else
	@echo "keyd target is Linux-only; skipping on $(UNAME)."
endif

# udev: stable /dev/apple-studio-display node + user access for asdcontrol.
.PHONY: udev
udev:
ifeq ($(UNAME),Linux)
	@echo "Linking udev rules to /etc/udev/rules.d (requires sudo)..."
	@sudo ln -fns $(DOTFILES)/linux/udev/50-apple-studio-display.rules /etc/udev/rules.d/50-apple-studio-display.rules
	@sudo udevadm control --reload-rules
	@sudo udevadm trigger --action=add --subsystem-match=usbmisc
	@echo "udev rules linked and reloaded."
else
	@echo "udev target is Linux-only; skipping on $(UNAME)."
endif

# --- Tool Installer Targets ---

setup-clojure-lsp: bin
	@echo "Installing/updating clojure-lsp..."
	@$(DOTFILES)/bin/setup-clojure-lsp $(ARGS)

setup-neil: bin
	@echo "Installing/updating neil..."
	@$(DOTFILES)/bin/setup-neil $(ARGS)

# --- Utility Targets ---

# Print the value of any make variable (e.g., make print-DOTFILES)
print-%:
	@echo '$* = $($*)'
