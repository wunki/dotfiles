require "plugins"
require "options"
require "utils"
require "mappings"

-- colors
vim.g.wunki_theme = "gruvbox"
local base16 = require "base16"
base16(base16.themes["nvchad-softgruv"], true)

-- custom colorss
require "highlights"

