call plug#begin('~/.config/nvim/plugged')

" Helper function to determine OS
if !exists("g:os")
  if has("win64") || has("win32") || has("win16")
    let g:os = "Windows"
  else
    let g:os = substitute(system('uname'), '\n', '', '')
  endif
endif

" Basics
Plug 'tpope/vim-repeat'                                  " make the . command available to more plugins
Plug 'tpope/vim-unimpaired'                              " bracket mappings for easy jumping
Plug 'tpope/vim-commentary'                              " comment mappings
Plug 'tpope/vim-surround'                                " surround commands
Plug 'airblade/vim-rooter'                               " automatically set the root path
Plug 'norcalli/nvim-colorizer.lua'                       " display colors nicely
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'} " better syntax formatting
Plug 'hrsh7th/nvim-compe'                                " autocomplete
Plug 'jiangmiao/auto-pairs'                              " automatically close pairs
Plug 'neomake/neomake'                                   " provide linting
Plug 'sbdchd/neoformat'                                  " formatting
augroup fmt
  autocmd!
  autocmd BufWritePre * try | undojoin | Neoformat | catch /^Vim\%((\a\+)\)\=:E790/ | finally | silent Neoformat | endtry
augroup END

" Git
Plug 'nvim-lua/plenary.nvim'
Plug 'lewis6991/gitsigns.nvim'

" Browsing files
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
  
" Colors
Plug 'sainnhe/everforest'
  let g:everforest_background = 'hard'
  let g:everforest_enable_italic = 1
Plug 'embark-theme/vim', { 'as': 'embark' }
  let g:embark_terminal_italics = 1
Plug 'itchyny/lightline.vim'
  let g:lightline = { 'colorscheme': 'embark' }

" Language server
Plug 'neovim/nvim-lspconfig'
Plug 'glepnir/lspsaga.nvim'
lua << EOF
vim.fn.sign_define("LspDiagnosticsSignError", {text = "", numhl = "LspDiagnosticsDefaultError"})
vim.fn.sign_define("LspDiagnosticsSignWarning", {text = "", numhl = "LspDiagnosticsDefaultWarning"})
vim.fn.sign_define("LspDiagnosticsSignInformation", {text = "", numhl = "LspDiagnosticsDefaultInformation"})
vim.fn.sign_define("LspDiagnosticsSignHint", {text = "", numhl = "LspDiagnosticsDefaultHint"})
EOF

" File explorer
Plug 'kyazdani42/nvim-web-devicons'
Plug 'kyazdani42/nvim-tree.lua'
  let g:nvim_tree_gitignore = 1
  nnoremap <C-n> :NvimTreeToggle<CR>
  nnoremap <leader>r :NvimTreeRefresh<CR>
  nnoremap <leader>n :NvimTreeFindFile<CR>

" Languages
Plug 'elixir-editors/vim-elixir'
Plug 'dag/vim-fish'

" Give me my parenthesis
Plug 'Olical/conjure', {'tag': 'v4.19.0'}
Plug 'tami5/compe-conjure'
Plug 'guns/vim-sexp'
Plug 'tpope/vim-sexp-mappings-for-regular-people'

call plug#end()

lua require('init')

" Setup language server
lua << EOF
require'lspconfig'.elixirls.setup{
    cmd = { vim.fn.expand("$HOME/.local/share/elixir-ls/release/language_server.sh") };
}
require'lspconfig'.clojure_lsp.setup{}
EOF
nnoremap <silent> gd <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> gr <cmd>lua vim.lsp.buf.references()<CR>
nnoremap <silent> K <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> gsd <cmd>lua vim.lsp.buf.document_symbol()<CR>
nnoremap <silent> gsw <cmd>lua vim.lsp.buf.workspace_symbol()<CR>

filetype plugin indent on

" Visuals
syntax on                   " enable syntax highlighting
set bg=dark                 " dark background
set cmdheight=2             " lower command line one lines high
set mouse=nv                " mouse in normal and visual mode
set modifiable              " needed for vimpager
set showmatch               " show matching brackets (), {}' []
set showcmd                 " show command
set modeline                " enable modelines
set ignorecase              " case-insensitive search
set smartcase               " upper-case sensitive search
set tabstop=2               " tab width is 4 spaces
set shiftwidth=2            " indent is also with 4 spaces
set expandtab               " expand tabs to spaces
set smarttab
set laststatus=2            " occasions to show status line, 2=always.
set hidden                  " move buffer to background without saving
set wrap                    " set long-line wrapping
set textwidth=80            " disable automatic text-width
set history=1000            " more history
set shortmess=atI           " shorten confirmation messages
set bs=2                    " fix backspace in FreeBSD
set scrolloff=3             " keep 3 lines below/above cursor
set noshowmode              " no need for the mode, lightline shows it
set showcmd                 " shows partial command in the last line
set ruler                   " show the line and column number of the cursor
set nonumber                " don't show line numbers
set backspace=indent,eol,start
set formatoptions=qrn1
set cursorline              " show me the line where the cursor is
set nofoldenable            " don't do any folding 
set guicursor+=a:blinkon0   " don't blink the cursor please
set clipboard=unnamedplus   " enable clipboard when on gui
set listchars=nbsp:¬,extends:»,precedes:«,trail:•
set showbreak=↪             " change wrap line break
set fillchars=diff:⣿,vert:│ " change fillchars
set splitright              " new windows are on the right
set splitbelow              " new windows are below the current one
if (has("termguicolors"))
 set termguicolors
endif
colorscheme embark

" Autocomplete configuration
source $HOME/.config/nvim/compe.vimrc

" Neomake for linting
let g:neomake_elixir_enabled_makers = ['credo']

" call Neomake when reading after 1s, and imm. after writing.
call neomake#configure#automake('rw', 1000)

" Ripgrep
set grepprg=rg\ --vimgrep
set grepformat=%f:%l:%c:%m

" Change mapleader to comma
let mapleader = " "
let maplocalleader = ","

" Use normal regular expressions
nnoremap / /\v
vnoremap / /\v

" Bash-like filename completion
set wildmenu
set wildmode=list:longest

" Backup
set undofile
set undodir=~/.config/nvim/tmp/undo/      " undo files
set backupdir=~/.config/nvim/tmp/backup/  " backups
set directory=~/.config/nvim/tmp/swap/    " swap files
set backup                                " enable backups
set noswapfile                            " no swapping

" Faster scrolling
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

" Shortcuts
inoremap hh <ESC>
nmap <silent> <Leader>l :set list!<CR>
noremap <silent> <leader>nn :set nu!<cr>
noremap <silent> <Leader>nr :set relativenumber!<cr>
nnoremap <C-l> :nohlsearch<CR><C-l>
nnoremap <leader>, :set invlist<cr>

" Left and right for switching between buffers
nnoremap <left> :bp<CR>
nnoremap <right> :bn<CR>

" Quick edit
nnoremap <leader>ev :vsplit $MYVIMRC<cr>

" Go up/down a display line, instead of actual line
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk

" Formatting
nnoremap Q gqip
vnoremap Q gq

" Terminal
tnoremap <Esc> <C-\><C-n>

" Browsing files
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>
nnoremap <leader>fr <cmd>Telescope oldfiles<cr>
nnoremap <leader>fs <cmd>Telescope git_status<cr>

" Mutt
au BufRead ~/.mutt/tmp/mutt-* set tw=72 formatoptions=tcql

" Make comments italic
hi Comment gui=italic cterm=italic
hi htmlArg gui=italic cterm=italic
