-- load all plugins
require "plugins"
local u = require "utils"
require "statusline"
require "neoformat"

require("colorizer").setup()
require("neoscroll").setup() -- smooth scroll

-- lsp stuff
require "nvim-lspconfig"
require "compe-completion"

local cmd = vim.cmd
local g = vim.g

g.mapleader = " "
g.auto_save = 0

-- colorscheme related stuff
cmd "syntax on"
g.gruvbox_material_enable_italic = 1
g.gruvbox_material_background = 'hard'

g.everforest_enable_italic = 1
g.everforest_background = 'hard'
cmd "colorscheme everforest"

-- blankline
g.indentLine_enabled = 1
g.indent_blankline_char = "▏"

g.indent_blankline_filetype_exclude = {"help", "terminal"}
g.indent_blankline_buftype_exclude = {"terminal"}

g.indent_blankline_show_trailing_blankline_indent = false
g.indent_blankline_show_first_indent_level = false

require "treesitter-nvim"
require "mappings"

require "telescope-nvim"
require "nvimTree" -- file tree stuff
require "file-icons"

-- git signs, lsp symbols etc
require "gitsigns-nvim"
require("nvim-autopairs").setup()
require("lspkind").init()

-- hide line numbers in terminal windows
vim.api.nvim_exec([[
   au BufEnter term://* setlocal nonumber
]], false)

-- set elixir as filetype, currently a bug in neovim
u.create_augroup({
    { 'BufRead,BufNewFile', 'mix.lock,*.exs,*.ex', 'setlocal', 'ft=elixir' },
    { 'BufRead,BufNewFile', '*.eex,*.leex', 'setlocal', 'ft=eelixir' },
}, 'elixir')

-- setup for TrueZen.nvim
require "zenmode"
require "whichkey"
