local packer = require('packer')
local use = packer.use

return packer.startup(function()
   use('wbthomason/packer.nvim')
   use('norcalli/nvim_utils')

   use({
      'akinsho/nvim-bufferline.lua',
      config = function()
         require('plugins.bufferline').config()
      end,
   })

   use({
      'glepnir/galaxyline.nvim',
      config = function()
         require('plugins.galaxyline').config()
      end,
   })

   -- colors
   use('siduck76/nvim-base16.lua')

   use({
      'sainnhe/gruvbox-material',
      event = 'VimEnter',
      config = function()
         vim.g.gruvbox_material_enable_italic = 1
         vim.g.gruvbox_material_background = 'hard'
         vim.cmd('colorscheme gruvbox-material')
      end,
      after = 'indent-blankline.nvim',
   })

   use({
      'norcalli/nvim-colorizer.lua',
      event = 'BufRead',
      config = function()
         require('colorizer').setup()
         vim.cmd('ColorizerReloadAllBuffers')
      end,
   })

   -- languages
   use({
      'nvim-treesitter/nvim-treesitter',
      event = 'BufRead',
      config = function()
         require('plugins.treesitter').config()
      end,
   })

   use({
      'neovim/nvim-lspconfig',
      after = "nvim-lspinstall",
      config = function()
         require('plugins.lspconfig').config()
      end,
   })

   use({
      'kabouzeid/nvim-lspinstall',
      event = 'BufRead',
   })

   use({
      'onsails/lspkind-nvim',
      event = 'BufRead',
      config = function()
         require('lspkind').init()
      end,
   })

   use('cespare/vim-toml')
   use({
      'dhruvasagar/vim-table-mode',
      cmd = {
         'TableModeToggle',
         'TableModeEnable',
         'TableModeDisable',
         'TableModeRealign',
         'TableSort',
         'Tableize',
      },
   })

   -- load compe in insert mode only
   use({
      'hrsh7th/nvim-compe',
      event = 'InsertEnter',
      config = function()
         require('plugins.compe').config()
      end,
      wants = { 'LuaSnip' },
      requires = {
         {
            'L3MON4D3/LuaSnip',
            wants = 'friendly-snippets',
            event = 'InsertCharPre',
            config = function()
               require('plugins.compe').snippets()
            end,
         },
         'rafamadriz/friendly-snippets',
      },
   })

   -- file managing, picker etc
   use({
      'kyazdani42/nvim-tree.lua',
      cmd = 'NvimTreeToggle',
      config = function()
         require('plugins.nvim-tree').config()
      end,
   })

   use({
      'kyazdani42/nvim-web-devicons',
      config = function()
         require('plugins.devicons').config()
      end,
   })

   use({
      'nvim-telescope/telescope.nvim',
      requires = {
         { 'nvim-lua/popup.nvim' },
         { 'nvim-lua/plenary.nvim' },
         { 'nvim-telescope/telescope-fzf-native.nvim', run = 'make' },
         { 'nvim-telescope/telescope-media-files.nvim' },
      },
      cmd = 'Telescope',
      config = function()
         require('plugins.telescope').config()
      end,
   })

   -- Git
   use({
      'lewis6991/gitsigns.nvim',
      event = 'BufRead',
      config = function()
         require('plugins.gitsigns').config()
      end,
   })

   use({
      'TimUntersberger/neogit',
      requires = 'nvim-lua/plenary.nvim',
      cmd = 'Neogit',
      config = function()
        local neogit = require("neogit")
        neogit.setup {
          disable_commit_confirmation = true
        }
      end
   })

   -- misc plugins
   use({
      'windwp/nvim-autopairs',
      after = 'nvim-compe',
      config = function()
         require('nvim-autopairs').setup()
         require('nvim-autopairs.completion.compe').setup({
            map_cr = true,
            map_complete = true, -- insert () func completion
         })
      end,
   })

   use({ 'andymass/vim-matchup', event = 'CursorMoved' })

   use({ 'jeffkreeftmeijer/vim-numbertoggle' })

   use({
      'terrortylor/nvim-comment',
      cmd = 'CommentToggle',
      config = function()
         require('nvim_comment').setup()
      end,
   })

   use({
      'glepnir/dashboard-nvim',
      cmd = {
         'Dashboard',
         'DashboardNewFile',
         'DashboardJumpMarks',
         'SessionLoad',
         'SessionSave',
      },
      setup = function()
         require('plugins.dashboard').config()
      end,
   })

   use({ 'tweekmonster/startuptime.vim', cmd = 'StartupTime' })

   -- load autosave only if its globally enabled
   use({
      'Pocco81/AutoSave.nvim',
      config = function()
         require('plugins.zenmode').autoSave()
      end,
      cond = function()
         return vim.g.auto_save == true
      end,
   })

   -- smooth scroll
   use({
      'karb94/neoscroll.nvim',
      event = 'WinScrolled',
      config = function()
         require('neoscroll').setup()
      end,
   })

   use({
      'Pocco81/TrueZen.nvim',
      cmd = { 'TZAtaraxis', 'TZMinimalist', 'TZFocus' },
      config = function()
         require('plugins.zenmode').config()
      end,
   })

   use({
     "folke/twilight.nvim",
     config = function()
       require("twilight").setup {
       }
     end
   })

   use({
      'lukas-reineke/indent-blankline.nvim',
      event = 'BufRead',
      setup = function()
         require('utils').blankline()
      end,
   })
end, {
   display = {
      border = { '┌', '─', '┐', '│', '┘', '─', '└', '│' },
   },
})
