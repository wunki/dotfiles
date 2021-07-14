local opt = vim.opt

opt.ruler = false
opt.hidden = true
opt.ignorecase = true
opt.splitbelow = true
opt.splitright = true
opt.termguicolors = true
opt.cul = true
opt.mouse = 'a'
opt.signcolumn = 'yes'
opt.cmdheight = 1
opt.updatetime = 250 -- update interval for gitsigns
opt.timeoutlen = 500
opt.clipboard = 'unnamedplus'

-- Numbers
opt.number = true
opt.numberwidth = 2
opt.relativenumber = true

-- for indenline
opt.expandtab = true
opt.shiftwidth = 2
opt.smartindent = true

-- disable builtin vim plugins
vim.g.loaded_gzip = 0
vim.g.loaded_tar = 0
vim.g.loaded_tarPlugin = 0
vim.g.loaded_zipPlugin = 0
vim.g.loaded_2html_plugin = 0
vim.g.loaded_netrw = 0
vim.g.loaded_netrwPlugin = 0
vim.g.loaded_matchit = 0
vim.g.loaded_matchparen = 0
vim.g.loaded_spec = 0

-- follow the leader, leader
vim.g.mapleader = ' '
vim.g.auto_save = false

require('nvim_utils')
local autocmds = {
   -- line numbers in the terminal.
   line_numbers = {
      { 'BufEnter', 'term://*', 'setlocal nonumber' },
      {
         'Bufenter,BufWinEnter,WinEnter,CmdwinEnter',
         '*',
         'if bufname(\'%\') == "NvimTree" | set laststatus=0 | else | set laststatus=2 | endif',
      },
      { 'BufEnter', 'term://*', 'set laststatus=0' },
   },

   -- indentation for languages
   tabbing = {
      { 'FileType', 'go', 'setlocal noexpandtab tabstop=4 shiftwidth=4 softtabstop=4' },
   },
}
nvim_create_augroups(autocmds)
