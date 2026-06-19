#!/usr/bin/env bash
set -euo pipefail

usage() {
  echo "usage: theme-sync [dark|light|toggle]" >&2
}

mode="${1:-toggle}"
case "$mode" in
  dark|light) ;;
  toggle)
    current="$(tmux show-environment -g TMUX_THEME 2>/dev/null | sed 's/^TMUX_THEME=//' || true)"
    if [[ "$current" == "light" ]]; then
      mode="dark"
    else
      mode="light"
    fi
    ;;
  *)
    usage
    exit 2
    ;;
esac

case "$mode" in
  dark)
    tmux_theme="$HOME/.tmux-zed-dark-theme.conf"
    nvim_scheme="gondolin"
    ;;
  light)
    tmux_theme="$HOME/.tmux-rose-pine-dawn-theme.conf"
    nvim_scheme="rose-pine-dawn"
    ;;
esac

if command -v tmux >/dev/null 2>&1 && tmux info >/dev/null 2>&1; then
  tmux set-environment -g TMUX_THEME "$mode"
  if [[ -r "$tmux_theme" ]]; then
    tmux source-file "$tmux_theme"
  else
    tmux display-message "Missing tmux theme: $tmux_theme"
  fi
fi

# Let interactive fish prompts on SSH/Linux use the same mode when system
# appearance detection is unavailable.
if command -v fish >/dev/null 2>&1; then
  fish -lc "set -Ux theme_mode_fallback $mode" >/dev/null 2>&1 || true
fi

sync_nvim() {
  local sock="$1"
  [[ -S "$sock" ]] || return 0
  command -v nvim >/dev/null 2>&1 || return 0

  local lua="(function() local mode = '$mode'; local scheme = '$nvim_scheme'; local ok, adm = pcall(require, 'auto-dark-mode'); local ok_interval, interval = pcall(require, 'auto-dark-mode.interval'); if ok_interval then interval.current_appearance = mode end; if ok and adm.options then if mode == 'dark' then adm.options.set_dark_mode() else adm.options.set_light_mode() end else vim.o.background = mode; pcall(vim.cmd.colorscheme, scheme) end; return true end)()"

  if command -v timeout >/dev/null 2>&1; then
    timeout 1s nvim --server "$sock" --remote-expr "luaeval(\"$lua\")" >/dev/null 2>&1 || true
  else
    nvim --server "$sock" --remote-expr "luaeval(\"$lua\")" >/dev/null 2>&1 || true
  fi
}

for sock in /tmp/nvim-* "${XDG_RUNTIME_DIR:-}/"nvim*; do
  sync_nvim "$sock"
done

if command -v tmux >/dev/null 2>&1 && tmux info >/dev/null 2>&1; then
  tmux display-message "Theme: $mode"
else
  echo "Theme: $mode"
fi
