# Dotfiles

These are the dotfiles I use across macOS, Linux, and FreeBSD. `make` links each tool's configuration into place, Cendre keeps the terminal stack visually consistent, and OS-specific or root-owned configuration stays isolated.

## Quick start

You need Git, Make, and the applications you want to configure. The Makefile links configuration; it does not install the applications themselves.

```bash
git clone git@github.com:wunki/dotfiles.git
cd dotfiles
make
```

### Ubuntu 26.04 workstation

On a clean Ubuntu 26.04 desktop, clone this repository and run the workstation bootstrap:

```bash
sudo apt update
sudo apt install --yes git
git clone https://github.com/wunki/dotfiles.git ~/Code/wunki/dotfiles
cd ~/Code/wunki/dotfiles
bin/setup-ubuntu
```

Use `bin/setup-ubuntu --dry-run` to review the work first. The script installs the core terminal workstation, Neovim, an upstream tmux build under `~/dev/tools/tmux`, mise's configured tools, Fish plugins, and the separate `wunki/dot-nvim` configuration. It is safe to rerun and deliberately does not change the login shell.

Ghostty comes from Ubuntu 26.04, Fish from its release-4 PPA, and `gh` from GitHub's official APT repository. Fonts, Zed, Sublime Text, Helix, Docker, PostgreSQL, and machine services are outside the bootstrap's scope.

`make` applies every user-level target. Run a specific target when you only want one tool:

```bash
make fish
make ghostty
make helix
make tmux
make pi
```

The Linux system targets require `sudo` and never run as part of `make`:

```bash
make linux          # keyd and udev
make auto-suspend   # desktop suspend timer
```

## What's here

| Area | Configuration |
| --- | --- |
| Shells | Fish and Zsh, split into shared and OS-specific files |
| Terminal | Ghostty and tmux |
| Editors | Helix, Zed, and Sublime Text |
| CLI tools | Bat, btop, Delta, eza, fzf, Lazygit, mise, and Herdr |
| AI tools | Shared agent instructions and skills, plus Claude Code, Codex, and Pi integration |
| Linux system | keyd remaps, Apple Studio Display access, and automatic suspend |
| Scripts | Tool installers, Neovim URL handling, Wake-on-LAN, and desktop suspend helpers |

### Theme

Cendre is the shared dark palette for Bat, btop, Delta, eza, fzf, Ghostty, Helix, Lazygit, Pi, and tmux. On macOS, tmux follows the system appearance and switches to Rose Pine Dawn in light mode.

Most Cendre files come from the theme's generated extras and remain unchanged here. Shell and Makefile wiring selects the right file for each tool.

## Installation targets

```bash
# Shells
make fish
make zsh

# Terminal and editors
make ghostty
make helix
make zed
make sublime
make tmux

# CLI tools
make bat
make btop
make delta
make eza
make fzf
make lazygit
make mise
make herdr

# AI tools
make agents
make claude
make codex
make pi

# Scripts in ~/.local/bin
make bin
```

`make btop` backs up an existing non-symlinked `~/.config/btop` directory before replacing it. `make delta` enables Delta with Cendre for regular Git diffs and adds `git hdiff` and `git hshow` for opening reviews in Hunk. It removes the legacy global `diff.external=difft` override when present; LazyGit keeps its explicit Difftastic command.

## Shell setup

### Shared tools

Install the tools you use before applying the shell configuration.

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

### Fish

Install [Fisher](https://github.com/jorgebucaran/fisher), then add the plugins used by this config:

```fish
curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source
fisher install jorgebucaran/fisher
fisher install jorgebucaran/autopair.fish
fisher install meaningful-ooo/sponge
fisher install PatrickF1/fzf.fish
fisher install IlanCosman/tide@v6
```

Run `tide configure` and choose the Lean layout with 16 colors. That leaves the palette to the terminal theme.

### Zsh

Install the external tools and clone the plugins:

```bash
brew install direnv mise gitu

mkdir -p ~/.zsh
git clone https://github.com/zsh-users/zsh-autosuggestions.git ~/.zsh/zsh-autosuggestions
git clone https://github.com/zdharma-continuum/fast-syntax-highlighting.git ~/.zsh/fast-syntax-highlighting
git clone https://github.com/hlissner/zsh-autopair.git ~/.zsh/zsh-autopair
git clone https://github.com/sindresorhus/pure.git ~/.zsh/pure
```

### mise-managed CLIs

Global npm CLIs use mise's npm backend rather than Node's deprecated default package files. This keeps them stable across Node upgrades.

```bash
mise use -g \
  'npm:hunkdiff@latest' \
  'npm:@earendil-works/pi-coding-agent@latest'
mise install
mise reshim
```

## AI coding tools

Shared instructions and skills live in `agents/`. The `petar-writing` skill captures the voice used on [petar.dev](https://petar.dev) and removes generic AI phrasing without inventing personal context.

```text
agents/
  AGENTS.md              shared working instructions
  skills/                shared Agent Skills

~/.agents                linked by make agents
~/.claude/skills         linked by make claude
~/.codex/AGENTS.md       linked by make codex
~/.pi/agent              assembled by make pi
```

Pi keeps its settings, models, extensions, and themes under `pi/agent/`. Claude and Codex only receive the shared pieces they support.

The repository includes skills for requirements gathering, structural search, code simplification, technical writing, PR descriptions, developer logs, tutoring, dependency updates, and atomic commits. The directory names under `agents/skills/` are the source of truth.

## Herdr

Herdr uses `Ctrl-h` as its prefix. Reload the configuration with `Ctrl-h r` or `herdr server reload-config`.

| Binding | Action |
| --- | --- |
| `Ctrl-h w` | Open the workspace picker |
| `Ctrl-h g` | Open the navigator |
| `Ctrl-h Shift-c` | Create a workspace |
| `Ctrl-h Shift-h` / `Ctrl-h Shift-l` | Move to the previous or next workspace |
| `Ctrl-h Shift-1..9` | Switch directly to workspace 1 through 9 |
| `j` / `k` in the navigator | Move between workspaces |
| `Ctrl-h ,` | Rename the tab |
| `Ctrl-h s` | Split horizontally |
| `Ctrl-h Shift-r` | Enter resize mode |
| `Ctrl-h Shift-s` | Open settings |

## Neovim URL handler

Neovim itself is configured in a separate repository. This one contains the macOS and tmux integration that opens browser stack traces in the correct Neovim process.

`nvs` starts Neovim with a socket at `/tmp/nvim-$SESSION_NAME`. A registered `nvim://` handler sends the file to that socket, selects the matching tmux pane, and focuses Ghostty. If the socket is unavailable, it opens a new Ghostty window instead.

Apply the scripts first:

```bash
make bin
```

The URL handler app lives at `~/Applications/Nvim URL Handler.app` and must register the `nvim` URL scheme. Its AppleScript source is `bin/NvimURLHandler.applescript`; app creation and URL registration are not automated by the Makefile.

Add the editor URL to the project's `.mise.toml`:

```toml
[env]
_.source = "~/.local/bin/mise-tmux-session"
PLUG_EDITOR = "nvim://file/__FILE__:__LINE__?tmux-session={{ env.TMUX_SESSION_NAME }}"
```

Start Neovim with `nvs` inside tmux.

## Keyboard repeat on GNOME

GNOME stores keyboard repeat settings in dconf rather than a dotfile, so these values must be applied on each machine.

| Key | Meaning |
| --- | --- |
| `repeat-interval` | Milliseconds between repeated characters. Lower is faster. Use `1` or higher because `0` can break Wayland login. |
| `delay` | Delay before repetition starts. Raise it when normal taps produce duplicate characters. |

My current values produce roughly 55 repeated characters per second without making normal taps unreliable:

```bash
gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 18
gsettings set org.gnome.desktop.peripherals.keyboard delay 200
```

The changes apply immediately. GNOME defaults are `repeat-interval 30` and `delay 500`.

## Linux system configuration

Most targets link files into the current user's home directory. Linux system configuration lives under `linux/` and has explicit installation targets because it writes to `/etc` or `/usr/local`.

```text
linux/
  keyd/default.conf                                      -> /etc/keyd/default.conf
  systemd/auto-suspend-monitor/auto-suspend-monitor      -> /usr/local/sbin/auto-suspend-monitor
  systemd/auto-suspend-monitor/auto-suspend-monitor.*    -> /etc/systemd/system/
  udev/50-apple-studio-display.rules                     -> /etc/udev/rules.d/50-apple-studio-display.rules
```

```bash
make linux          # keyd and udev
make keyd           # keyboard remaps and Studio Display brightness keys
make udev           # stable Studio Display device and user access
make auto-suspend   # automatic desktop suspend
```

### Automatic suspend

`make auto-suspend` installs a systemd timer that checks activity once per minute. It suspends the desktop after 30 minutes without an active session.

Active SSH, Tailscale SSH, Zed remote, console, and non-idle graphical sessions reset the countdown. Sessions stuck in `closing` do not. Sleep and idle inhibitors prevent suspension.

Before suspending, the monitor writes `~/.cache/dotfiles-desktop-sleep/slept-at`. `sleep-desktop` writes the same timestamp when suspension is requested manually. `wake-desktop` consumes it after the machine returns and reports the sleep duration.

The installer requires `shellcheck`, Python 3, and `runuser`. It validates the monitor, installs root-owned copies, reloads systemd, and enables the timer.

```bash
systemctl status auto-suspend-monitor.timer
journalctl -t auto-suspend-monitor
```

### Apple Studio Display brightness

The Studio Display exposes no `/sys/class/backlight` device and does not support DDC/CI. Brightness is available through Apple's USB HID protocol using [`asdcontrol`](https://github.com/nikosdion/asdcontrol), installed at `/usr/local/bin/asdcontrol`.

- `bin/asd-brightness` supports `up`, `down`, and `get`, with a 6% step.
- `linux/udev/50-apple-studio-display.rules` exposes `/dev/apple-studio-display` with `users` group access. It targets USB interface 07, the display's brightness HID.
- `linux/keyd/default.conf` maps the keyboard's `F15` and `F14` events to `asd-brightness`. This works outside the desktop environment and does not require `sudo` at runtime.

Install both pieces with `make keyd udev` or `make linux`. Change `STEP` in `bin/asd-brightness` to adjust the increment.

## Troubleshooting

**A target did not create its symlink.** Check permissions on the destination and run that target again. Some targets back up an existing real directory rather than replacing it silently.

**Fish plugins are missing.** Install Fisher, run the plugin commands above, and restart Fish.

**Cendre is not active.** Confirm the tool's config directory points into this repository. For Bat, run `bat cache --build` after adding or updating the theme.

**An OS-specific Fish file did not load.** Run `status buildinfo` and compare its target with the cases in `fish/config.fish`.

## License

The repository is marked as MIT, but it does not currently include a `LICENSE` file.
