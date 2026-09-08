DOTFILES	:= $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
CONFIG_DIR	:= ${HOME}/.config
GNOME_EXTENSIONS_DIR := $(HOME)/.local/share/gnome-shell/extensions
APPLICATION_SHORTCUTS_UUID := application-shortcuts@wunki
APPLICATION_SHORTCUTS_DIR := $(DOTFILES)/gnome-shell/$(APPLICATION_SHORTCUTS_UUID)
UNAME		:= $(shell uname -s)

# User-level targets included in `make`.
APP_TARGETS := fish zsh bat btop delta eza fzf helix ghostty gtk hunk zed sublime tmux bin lazygit mise agents claude codex pi

.PHONY: all
all: $(APP_TARGETS)
	@echo "All user-level dotfiles linked."

.PHONY: $(APP_TARGETS) print-% ensure-config-dir setup-clojure-lsp setup-neil setup-neovim setup-ubuntu

# --- Application configuration ---

# Move a real file or directory aside before replacing it with a symlink.
define backup_real_path
	@if [ -e "$(1)" ] && [ ! -L "$(1)" ]; then \
		backup="$(1).bak.$$(date +%Y%m%d%H%M%S)"; \
		mv "$(1)" "$$backup"; \
		echo "Backed up existing $(2) to $$backup"; \
	fi
endef

ensure-config-dir:
	@echo "Ensuring $(CONFIG_DIR) exists..."
	@mkdir -p $(CONFIG_DIR)

fish: ensure-config-dir
	@echo "Linking fish configuration..."
	@ln -fns $(DOTFILES)/fish $(CONFIG_DIR)/fish
	@echo "Fish linked. Run 'fisher update' to install the plugins in fish_plugins."

zsh:
	@echo "Linking zsh configuration..."
	@ln -fns $(DOTFILES)/zsh/zshrc $(HOME)/.zshrc
ifeq ($(UNAME),Darwin)
	@ln -fns $(DOTFILES)/zsh/zshrc.mac $(HOME)/.zshrc.mac
	@echo "ZSH linked (including macOS specific file)."
else
	@echo "ZSH linked."
endif

bat: ensure-config-dir
	@echo "Linking bat configuration..."
	@ln -fns $(DOTFILES)/bat $(CONFIG_DIR)/bat
	@if command -v bat >/dev/null 2>&1; then \
		bat cache --build; \
		echo "Bat linked and theme cache rebuilt."; \
	else \
		echo "Bat linked; install bat to build the theme cache."; \
	fi

btop: ensure-config-dir
	@echo "Linking btop configuration..."
	$(call backup_real_path,$(CONFIG_DIR)/btop,btop configuration)
	@ln -fns $(DOTFILES)/btop $(CONFIG_DIR)/btop
	@echo "btop linked."

delta: ensure-config-dir
	@echo "Linking Delta configuration..."
	@ln -fns $(DOTFILES)/delta $(CONFIG_DIR)/delta
	@if command -v git >/dev/null 2>&1; then \
		if [ "$$(git config --global --get diff.external)" = "difft" ]; then \
			git config --global --unset-all diff.external; \
		fi; \
		for config_file in cendre.gitconfig git.gitconfig; do \
			config_path="~/.config/delta/$$config_file"; \
			git config --global --get-all include.path | grep -Fxq "$$config_path" || \
				git config --global --add include.path "$$config_path"; \
		done; \
	fi
	@echo "Delta linked and enabled with Cendre in the global Git configuration."

eza: ensure-config-dir
	@echo "Linking eza theme..."
	@ln -fns $(DOTFILES)/eza $(CONFIG_DIR)/eza
	@echo "eza linked."

fzf: ensure-config-dir
	@echo "Linking fzf theme..."
	@ln -fns $(DOTFILES)/fzf $(CONFIG_DIR)/fzf
	@echo "fzf linked."

helix: ensure-config-dir
	@echo "Linking helix configuration..."
	@ln -fns $(DOTFILES)/helix $(CONFIG_DIR)/helix
	@echo "Helix linked."

ghostty: ensure-config-dir
	@echo "Linking ghostty configuration..."
	@ln -fns $(DOTFILES)/ghostty $(CONFIG_DIR)/ghostty
	@echo "Ghostty linked."

gtk: ensure-config-dir
	@echo "Linking GTK 4 Cendre theme override..."
	@mkdir -p $(CONFIG_DIR)/gtk-4.0
	@ln -fns $(DOTFILES)/gtk-4.0/gtk.css $(CONFIG_DIR)/gtk-4.0/gtk.css
ifeq ($(UNAME),Linux)
	@echo "Linking Cendre GNOME Shell theme..."
	@mkdir -p $(HOME)/.themes
	$(call backup_real_path,$(HOME)/.themes/cendre,GNOME Shell theme)
	@ln -fns $(DOTFILES)/gnome-shell/cendre $(HOME)/.themes/cendre
	@echo "Linking GNOME application shortcuts extension..."
	@glib-compile-schemas $(APPLICATION_SHORTCUTS_DIR)/schemas
	@mkdir -p $(GNOME_EXTENSIONS_DIR)
	$(call backup_real_path,$(GNOME_EXTENSIONS_DIR)/$(APPLICATION_SHORTCUTS_UUID),application shortcuts extension)
	@ln -fns $(APPLICATION_SHORTCUTS_DIR) $(GNOME_EXTENSIONS_DIR)/$(APPLICATION_SHORTCUTS_UUID)
	@if command -v gsettings >/dev/null 2>&1 && \
		gsettings list-schemas | grep -Fxq org.gnome.shell.extensions.user-theme; then \
		enabled="$$(gsettings get org.gnome.shell enabled-extensions)"; \
		for uuid in user-theme@gnome-shell-extensions.gcampax.github.com $(APPLICATION_SHORTCUTS_UUID); do \
			if ! printf '%s\n' "$$enabled" | grep -Fq "'$$uuid'"; then \
				case "$$enabled" in \
					"@as []"|"[]") enabled="['$$uuid']" ;; \
					*) enabled="$${enabled%]}"; enabled="$$enabled, '$$uuid']" ;; \
				esac; \
			fi; \
		done; \
		gsettings set org.gnome.shell enabled-extensions "$$enabled"; \
		gsettings set org.gnome.shell.extensions.user-theme name cendre; \
		echo "GNOME Shell configured; log out once if an extension was just installed."; \
	else \
		echo "GNOME Shell theme linked but inactive; install gnome-shell-extension-user-theme."; \
	fi
endif
	@echo "GTK 4 and GNOME Shell Cendre themes linked."

hunk: ensure-config-dir
	@echo "Linking Hunk configuration..."
	@mkdir -p $(CONFIG_DIR)/hunk
	@ln -fns $(DOTFILES)/hunk/config.toml $(CONFIG_DIR)/hunk/config.toml
	@echo "Hunk linked."

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
	$(call backup_real_path,$(HOME)/.agents,~/.agents)
	@ln -fns $(DOTFILES)/agents $(HOME)/.agents
	@echo "Shared agents linked."

claude: agents
	@echo "Linking shared skills for Claude..."
	@mkdir -p $(HOME)/.claude
	$(call backup_real_path,$(HOME)/.claude/skills,Claude skills)
	@ln -fns $(DOTFILES)/agents/skills $(HOME)/.claude/skills
	@echo "Claude skills linked."

codex: agents
	@echo "Linking Codex global instructions and theme..."
	@mkdir -p $(HOME)/.codex
	$(call backup_real_path,$(HOME)/.codex/AGENTS.md,~/.codex/AGENTS.md)
	@ln -fns $(DOTFILES)/agents/AGENTS.md $(HOME)/.codex/AGENTS.md
	@$(DOTFILES)/codex/install-theme
	@echo "Codex linked."

pi:
	@echo "Linking Pi global configuration..."
	@mkdir -p $(HOME)/.pi/agent
	@ln -fns $(DOTFILES)/agents/AGENTS.md $(HOME)/.pi/agent/AGENTS.md
	$(call backup_real_path,$(HOME)/.pi/agent/extensions,Pi extensions)
	@ln -fns $(DOTFILES)/pi/agent/extensions $(HOME)/.pi/agent/extensions
	$(call backup_real_path,$(HOME)/.pi/agent/themes,Pi themes)
	@ln -fns $(DOTFILES)/pi/agent/themes $(HOME)/.pi/agent/themes
	@ln -fns $(DOTFILES)/pi/agent/settings.json $(HOME)/.pi/agent/settings.json
	@ln -fns $(DOTFILES)/pi/agent/models.json $(HOME)/.pi/agent/models.json
	@echo "Pi linked."

# --- Linux system configuration ---
#
# These root-owned targets only run on Linux and are not part of `make`.
# Source files live under linux/<tool>/; each target names its destination.

.PHONY: linux
linux: keyd udev
	@echo "Linux system configuration linked."

# Suspend this desktop after 30 minutes without an active session.
# systemd runs the monitor as root, so install root-owned copies.
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

# Install keyd remaps and Apple Studio Display brightness keys.
# `bin` runs first because F15/F14 call asd-brightness from ~/.local/bin.
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

# Expose /dev/apple-studio-display and grant asdcontrol access.
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

# --- Tool installers ---

setup-ubuntu:
	@$(DOTFILES)/bin/setup-ubuntu $(ARGS)

setup-neovim: bin
	@echo "Installing/updating Neovim..."
	@$(DOTFILES)/bin/setup-neovim $(ARGS)

setup-clojure-lsp: bin
	@echo "Installing/updating clojure-lsp..."
	@$(DOTFILES)/bin/setup-clojure-lsp $(ARGS)

setup-neil: bin
	@echo "Installing/updating neil..."
	@$(DOTFILES)/bin/setup-neil $(ARGS)

# --- Utilities ---

# Print a Make variable, for example `make print-DOTFILES`.
print-%:
	@echo '$* = $($*)'
