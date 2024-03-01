-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- Default to Fish shell
config.default_prog = { '/opt/homebrew/bin/fish', '-l' }

-- Colorscheme
config.color_scheme = 'Kanagawa (Gogh)'

-- Font
config.font = wezterm.font 'BerkeleyMono Nerd Font Mono'
config.font_size = 13.0

-- Disable the tab bar since I use tmux
config.enable_tab_bar = false

return config
