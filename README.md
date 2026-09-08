# Dotfiles

These are the dotfiles I use on macOS, Linux, and FreeBSD. `make` links the configuration of each tool into place. Cendre, the shared color theme, gives the terminal tools the same look. Configuration that belongs to one OS, or that root owns, stays in its own directory.

## Quick start

You need Git, Make, and the applications that you want to configure. The Makefile links configuration. It does not install the applications.

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

If you want to see the planned work first, run `bin/setup-ubuntu --dry-run`. The script installs the core terminal workstation, Neovim, an upstream tmux build under `~/dev/tools/tmux`, the tools that mise manages, the Fish plugins, and the separate `wunki/dot-nvim` configuration. You can run the script again at any time. It does not change the login shell.

Fish comes from its official standalone release. `gh` comes from the official APT repository of GitHub. The bootstrap does not install fonts, terminal emulators, Zed, Sublime Text, Helix, Docker, PostgreSQL, or machine services.

`make` applies every user-level target. If you want one tool only, run its target:

```bash
make fish
make ghostty
make gtk
make helix
make tmux
make pi
```

The Linux system targets need `sudo`. `make` never runs them:

```bash
make linux          # keyd and udev
make auto-suspend   # desktop suspend timer
```

## What is here

| Area | Configuration |
| --- | --- |
| Shells | Fish and Zsh, split into shared and OS-specific files |
| Desktop and terminal | GTK 4, GNOME Shell, Ghostty, and tmux |
| Editors | Helix, Zed, and Sublime Text |
| CLI tools | Bat, btop, Delta, eza, fzf, Hunk, Lazygit, and mise |
| AI tools | Shared agent instructions and skills, plus Claude Code, Codex, and Pi integration |
| Linux system | keyd remaps, Apple Studio Display access, and automatic suspend |
| Scripts | Tool installers, Neovim URL handling, Wake-on-LAN, and desktop suspend helpers |

### Theme

Cendre is the shared dark palette for Bat, btop, Delta, eza, fzf, Ghostty, GTK 4, GNOME Shell, Helix, Hunk, Lazygit, Pi, and tmux. The earlier tmux themes stay under `tmux/themes/`, so you can switch back later.

Most Cendre files come from the generated extras of the theme. This repository does not change them. The shell and Makefile wiring selects the correct file for each tool.

## Installation targets

```bash
# Shells
make fish
make zsh

# Terminal and editors
make ghostty
make gtk
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
make hunk

# AI tools
make agents
make claude
make codex
make pi

# Scripts in ~/.local/bin
make bin
```

If `~/.config/btop` is a real directory and not a symlink, `make btop` backs it up before it replaces it. `make delta` enables Delta with Cendre for normal Git diffs. It also adds `git hdiff` and `git hshow`, which open a review in Hunk. If the legacy global `diff.external=difft` override is present, the target removes it. LazyGit keeps its own Difftastic command.

`make gtk` links the GTK 4 overrides into `~/.config/gtk-4.0/gtk.css`. On Linux, it also links the GNOME Shell theme into `~/.themes/cendre`. It selects Cendre through the User Themes extension and keeps the rest of the enabled extensions. The Ubuntu bootstrap installs that extension. After the first installation of the extension, log out once before you expect the shell theme to appear. Restart GTK 4 applications to update their window chrome.

## Shell setup

### Shared tools

Install the tools that you use before you apply the shell configuration.

```bash
# macOS
brew install eza bat fzf zoxide tree autossh mise gitu

# Debian/Ubuntu
sudo apt install eza bat fzf zoxide tree autossh git gh tmux

# Fedora
sudo dnf install eza bat fzf zoxide tree autossh git gh tmux

# Arch
sudo pacman -S eza bat fzf zoxide tree autossh github-cli tmux
```

### Fish

Install [Fisher](https://github.com/jorgebucaran/fisher), the Fish plugin manager. Then install the plugins listed in `fish/fish_plugins`:

```fish
curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source
fisher update
```

Run `tide configure` and select the Lean layout with 16 colors. The terminal theme then controls the palette.

### Zsh

Install the external tools and clone the plugins:

```bash
brew install mise gitu

mkdir -p ~/.zsh
git clone https://github.com/zsh-users/zsh-autosuggestions.git ~/.zsh/zsh-autosuggestions
git clone https://github.com/zdharma-continuum/fast-syntax-highlighting.git ~/.zsh/fast-syntax-highlighting
git clone https://github.com/hlissner/zsh-autopair.git ~/.zsh/zsh-autopair
git clone https://github.com/sindresorhus/pure.git ~/.zsh/pure
```

### mise-managed CLIs

Global npm CLIs use the npm backend of mise, not the deprecated default package files of Node. The CLIs then keep working when Node upgrades.

```bash
mise use -g \
  'npm:hunkdiff@latest' \
  'npm:@earendil-works/pi-coding-agent@latest'
mise install
mise reshim
```

## AI coding tools

The shared instructions and skills live in `agents/`. The `petar-writing` skill describes the voice used on [petar.dev](https://petar.dev). It removes generic AI phrasing and does not invent personal context.

```text
agents/
  AGENTS.md              shared working instructions
  skills/                shared Agent Skills

~/.agents                linked by make agents
~/.claude/skills         linked by make claude
~/.codex/AGENTS.md       linked by make codex
~/.pi/agent              assembled by make pi
```

Pi keeps its configuration, models, extensions, and themes under `pi/agent/`. Claude and Codex receive only the shared pieces that they support.

The repository includes skills for requirements gathering, structural search, code simplification, technical and personal writing, PR descriptions, developer logs, tutoring, Elixir dependency updates, atomic commits, fetching GitHub files, and rendering reMarkable PDFs. The directory names under `agents/skills/` are the source of truth.

### Image paste over SSH

CleanShot X copies a local macOS path together with the screenshot. That path does not exist on an SSH host. [cc-clip](https://github.com/ShunmeiCho/cc-clip) sends the clipboard image through an SSH reverse tunnel instead. The tmux binding then writes the image to a real remote path and pastes that path into the active pane.

Install the local pieces on the Mac:

```bash
brew install pngpaste
curl -fsSL https://raw.githubusercontent.com/ShunmeiCho/cc-clip/main/scripts/install.sh | sh
cc-clip service install
```

Add the tunnel to the named host in `~/.ssh/config`:

```sshconfig
Host desktop
  RemoteForward 18339 127.0.0.1:18339
  ControlMaster no
  ControlPath none
```

Deploy only the remote clipboard transport. The `--claude` target installs the `xclip` shim that Claude Code and Pi use. It does not make the tmux path specific to Claude. `--no-notify` skips the agent notification integrations. cc-clip still creates a session ID that it uses for image-transfer notifications. If you want all notifications off, remove that file after every connect or redeploy:

```bash
cc-clip connect desktop --claude --no-notify
ssh desktop 'rm -f ~/.cache/cc-clip/session.id ~/.cache/cc-clip/notify.nonce'
```

Apply the tmux configuration on the remote:

```bash
ssh desktop
cd ~/Code/wunki/dotfiles
make tmux
tmux source-file ~/.tmux.conf
```

Copy an image in CleanShot X. Then press `Ctrl-h Shift-I` inside the remote tmux session. The binding is active when tmux has `SSH_CONNECTION`. It runs `cc-clip paste --out-dir /tmp/screenshots` and pastes the remote path into the current pane. Codex and other CLIs that accept image paths can read the file without Xvfb or `DISPLAY`. Pi can also use its normal `Ctrl+V` path through the installed `xclip` shim.

The Mac daemon listens only on `127.0.0.1:18339`. SSH exposes it on the loopback interface of the remote. The token in `~/.cache/cc-clip/session.token` authenticates each request. cc-clip does not select the server. The SSH `Host` block creates the tunnel, and the `cc-clip paste` process writes the file on the remote host that runs it.

Keep an SSH connection open while you use the binding. A tmux session can survive a disconnect, but the reverse tunnel cannot. The first live SSH connection owns port `18339`. A second connection can report `remote port forwarding failed for listen port 18339` and still use the tunnel of the first connection.

## Neovim URL handler

Neovim itself is configured in a separate repository. This repository contains the macOS and tmux integration that opens a stack trace from the browser in the correct Neovim process.

`nvs` starts Neovim with a socket at `/tmp/nvim-$SESSION_NAME`. A registered `nvim://` handler sends the file to that socket, selects the matching tmux pane, and focuses Ghostty. If the socket is not available, the handler opens a new Ghostty window instead.

Apply the scripts first:

```bash
make bin
```

The URL handler app lives at `~/Applications/Nvim URL Handler.app` and must register the `nvim` URL scheme. Its AppleScript source is `bin/NvimURLHandler.applescript`. The Makefile does not create the app and does not register the URL.

Add the editor URL to the `.mise.toml` of the project:

```toml
[env]
_.source = "~/.local/bin/mise-tmux-session"
PLUG_EDITOR = "nvim://file/__FILE__:__LINE__?tmux-session={{ env.TMUX_SESSION_NAME }}"
```

Start Neovim with `nvs` inside tmux.

## GNOME input settings

This section records the keyboard repeat, mouse movement, and scrolling values that I use on the Ubuntu workstation.

### Keyboard repeat

GNOME stores the keyboard repeat values in dconf, its settings database, and not in a dotfile. You must apply these values on each machine.

| Key | Meaning |
| --- | --- |
| `repeat-interval` | Milliseconds between repeated characters. A lower value is faster. Use `1` or higher, because `0` can break the Wayland login. |
| `delay` | Delay before the repeat starts. If normal taps produce double characters, raise it. |

My current values give about 55 repeated characters per second, and normal taps stay reliable:

```bash
gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 18
gsettings set org.gnome.desktop.peripherals.keyboard delay 200
```

The changes apply at once. The GNOME defaults are `repeat-interval 30` and `delay 500`.

### Notification banner position

GNOME shows notification banners at the top center and has no setting to move them. The Just Perfection extension adds one and stores it in dconf. Install the extension on each machine, then apply the setting.

```bash
gsettings --schemadir ~/.local/share/gnome-shell/extensions/just-perfection-desktop@just-perfection/schemas \
  set org.gnome.shell.extensions.just-perfection notification-banner-position 2
```

The values are `0` top left, `1` top center (default), `2` top right, `3` bottom left, `4` bottom center, and `5` bottom right. The change applies at once.

### MX Master 4 cursor and scrolling

Let the Linux HID++ driver manage the wheel resolution of the MX Master 4. If you force `Scroll Wheel Resolution` on in Solaar, the movement is smooth but much too fast. Solaar and the kernel then both try to control the same device setting.

Install Solaar, the Logitech device manager for Linux. Then refresh the permissions of the Bolt receiver that is already connected. If you can disconnect the mouse safely, you can also unplug and replug the receiver after the installation. The result is the same.

```bash
sudo apt install solaar
sudo udevadm control --reload-rules
sudo udevadm trigger --action=add --subsystem-match=hidraw
udevadm settle
```

Open Solaar. Click the icon at the right edge of `Scroll Wheel Resolution` until its mode reads `Ignore this setting`. This is the default for new devices. After you change the mode, turn the mouse off and on, so that the kernel can reset the wheel.

Make sure that Solaar will not touch the setting:

```bash
rg _sensitive ~/.config/solaar/config.yaml
```

The result must include `hires-smooth-resolution: ignore`. The nearby `hires-smooth-resolution: true` or `false` line records the last value that Solaar saw. It does not override the device while the sensitivity mode is `ignore`.

Firefox already enables smooth scrolling and mass-spring-damper physics. Keep `mousewheel.default.delta_multiplier_y` at its default value of `100`. A higher value makes each wheel movement travel farther. If scrolling becomes too fast again, make sure that the Solaar mode is still `ignore`, turn the mouse off and on, and reset that Firefox preference from `about:config`.

The current baseline keeps pointer movement responsive on the 5K display and does not change the wheel behavior:

| Setting | Value | Reason |
| --- | --- | --- |
| Connection | Logi Bolt receiver | Exposes the mouse to Solaar and avoids Bluetooth-specific configuration differences. |
| Sensitivity | 1200 DPI | A 20% increase over the 1000 DPI default of Logitech. Set `Sensitivity (DPI)` in Solaar. |
| GNOME acceleration | Default profile, speed `0.573` | Keeps adaptive acceleration and fine control. Raise the DPI before you push the desktop speed slider further. |
| Natural scrolling | On | Matches the macOS content direction. |
| Wheel resolution | Ignore in Solaar | Leaves smooth scrolling to the Linux HID++ driver. |
| Wheel mode | Ratcheted, torque `75`, switch speed `12` | Keeps clear steps and keeps the physical free-spin toggle. These values are about feel, not tracking accuracy. |
| Wheel diversion | Off | Sends standard wheel events directly to applications. |
| Haptic feedback | `60` | Current preference. It does not affect pointer or wheel tracking. |

Logitech supports 200 to 8000 DPI in steps of 50 DPI. When you tune further, change the DPI in steps of 50 to 100 points. Change one layer at a time, so that you can judge the result.

## Linux system configuration

Most targets link files into the home directory of the current user. The Linux system configuration lives under `linux/` and has its own installation targets, because it writes to `/etc` or `/usr/local`.

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

`make auto-suspend` installs a systemd timer that checks for activity once per minute. If there is no active session for 30 minutes, it suspends the desktop.

Active SSH, Tailscale SSH, Zed remote, console, and non-idle graphical sessions reset the countdown. Sessions stuck in `closing` do not. Sleep and idle inhibitors prevent the suspend.

Before the suspend, the monitor writes `~/.cache/dotfiles-desktop-sleep/slept-at`. `sleep-desktop` writes the same timestamp when you request the suspend by hand. `wake-desktop` reads and removes it after the machine returns, and reports how long the machine slept.

When the machine runs Windows with WSL2, `sleep-wsl` calls the Windows suspend API from an SSH session inside WSL. `wake-wsl` sends Wake-on-LAN through the Pi, waits for WSL SSH, and reports how long the machine slept. Windows must expose S3 sleep, and its Ethernet adapter must be armed for wake.

The installer needs `shellcheck`, Python 3, and `runuser`. It validates the monitor, installs root-owned copies, reloads systemd, and enables the timer.

```bash
systemctl status auto-suspend-monitor.timer
journalctl -t auto-suspend-monitor
```

### Apple Studio Display brightness

The Studio Display has no `/sys/class/backlight` device and does not support DDC/CI. Brightness is available through the USB HID protocol of Apple with [`asdcontrol`](https://github.com/nikosdion/asdcontrol), installed at `/usr/local/bin/asdcontrol`.

- `bin/asd-brightness` supports `up`, `down`, and `get`, with a step of 6%.
- `linux/udev/50-apple-studio-display.rules` exposes `/dev/apple-studio-display` with access for the `users` group. It targets USB interface 07, the brightness HID of the display.
- `linux/keyd/default.conf` maps the `F15` and `F14` events of the keyboard to `asd-brightness`. This works outside the desktop environment and does not need `sudo` at runtime.

Install both pieces with `make keyd udev` or `make linux`. To change the step size, change `STEP` in `bin/asd-brightness`.

## Troubleshooting

### A target did not create its symlink

Make sure that you can write to the destination, then run the target again. Some targets back up an existing real directory instead of replacing it without notice.

### Fish plugins are missing

Install Fisher, run `fisher update`, and restart Fish.

### Cendre is not active

Make sure that the configuration directory of the tool points into this repository. For Bat, run `bat cache --build` after you add or update the theme.

### An OS-specific Fish file did not load

Run `status buildinfo` and compare its target with the cases in `fish/config.fish`.

## License

MIT. See [LICENSE](LICENSE).
