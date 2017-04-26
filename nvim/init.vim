let $NVIM_TUI_ENABLE_TRUE_COLOR = 1

call plug#begin('~/.config/nvim/plugged')

" Essentials
Plug 'w0rp/ale'
  let g:ale_lint_on_save = 1
  let g:ale_lint_on_text_changed = 0
Plug 'tpope/vim-eunuch'                                  " unix helper commands
Plug 'tpope/vim-repeat'                                  " make the . command available to more plugins
Plug 'tpope/vim-unimpaired'                              " bracket mappings for easy jumping
Plug 'tpope/vim-obsession'                               " better vim sessions
Plug 'tpope/vim-speeddating'                             " easily increment numbers and dates
Plug 'tpope/vim-endwise'                                 " automatically close blocks in languages like ruby or elixir
Plug 'airblade/vim-gitgutter'                            " show git changes in the gutter
Plug 'rking/ag.vim'                                      " silver searcher
Plug 'mattn/webapi-vim'
Plug 'mattn/gist-vim'                                    " post to gist
Plug 'tpope/vim-commentary'                              " comment mappings
Plug 'tpope/vim-surround'                                " surround commands
Plug 'godlygeek/tabular'                                 " easy indenting
Plug 'Lokaltog/vim-easymotion'                           " move by selecting a letter
Plug 'mbbill/undotree'                                   " easy undoing
  nnoremap <silent> <Leader>ut :UndotreeToggle<cr>
Plug 'jiangmiao/auto-pairs'                              " pair parenthesis, brackend and quotes
Plug 'dhruvasagar/vim-table-mode', { 'for': 'markdown' } " table creation in markdown
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
  map <C-p> :Files<cr>
  nmap <C-p> :Files<cr>
  nmap <Leader>c :Commits<cr>
Plug 'junegunn/fzf.vim'
  let g:fzf_colors =
  \ { 'fg':      ['fg', 'Normal'],
    \ 'bg':      ['bg', 'Normal'],
    \ 'hl':      ['fg', 'Comment'],
    \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
    \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
    \ 'hl+':     ['fg', 'Statement'],
    \ 'info':    ['fg', 'PreProc'],
    \ 'prompt':  ['fg', 'Conditional'],
    \ 'pointer': ['fg', 'Exception'],
    \ 'marker':  ['fg', 'Keyword'],
    \ 'spinner': ['fg', 'Label'],
    \ 'header':  ['fg', 'Comment'] }
Plug 'Shougo/vimproc', {'do': 'make'}                    " command execution
Plug 'Shougo/echodoc.vim'                                " show function signature
Plug 'aliva/vim-fish', { 'for': 'fish' }
Plug 'vitalk/vim-simple-todo'                            " simple todo's
Plug 'othree/html5.vim', { 'for': 'html' }
Plug 'majutsushi/tagbar'                                 " sidebar to jump to regions
  map <C-t> :Tagbar<CR>
  let g:tagbar_type_markdown = {
    \ 'ctagstype' : 'markdown',
    \ 'kinds' : [
        \ 'h:Heading_L1',
        \ 'i:Heading_L2',
        \ 'k:Heading_L3'
    \ ]
  \ }

  let g:tagbar_type_rust = {
    \ 'ctagstype' : 'rust',
    \ 'kinds' : [
        \'T:types,type definitions',
        \'f:functions,function definitions',
        \'g:enum,enumeration names',
        \'s:structure names',
        \'m:modules,module names',
        \'c:consts,static constants',
        \'t:traits,traits',
        \'i:impls,trait implementations',
    \]
  \}

" Autocomplete
Plug 'Shougo/deoplete.nvim'
    let g:acp_enableAtStartup = 0
    let g:deoplete#enable_at_startup = 1
    let g:deoplete#enable_ignore_case = 'ignorecase'
    let g:deoplete#sources#syntax#min_keyword_length = 3
    inoremap <silent><expr> <Tab> pumvisible() ? "\<C-n>" : deoplete#mappings#manual_complete()
    inoremap <Leader><Tab> <Space><Space>

Plug 'arcticicestudio/nord-vim'
Plug 'morhetz/gruvbox'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-airline/vim-airline'
    let g:airline_powerline_fonts = 0
    let g:airline_left_sep = ''
    let g:airline_right_sep = ''

" Rust
Plug 'rust-lang/rust.vim', { 'for': 'rust' }
Plug 'cespare/vim-toml', { 'for': 'toml' }

" Go
Plug 'fatih/vim-go'

" Elixir
Plug 'elixir-lang/vim-elixir'
Plug 'slashmili/alchemist.vim'

" Language server
Plug 'autozimu/LanguageClient-neovim', { 'do': ':UpdateRemotePlugins' }
let g:LanguageClient_serverCommands = {
    \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
    \ }
let g:LanguageClient_autoStart = 1

" Mac only
if has("mac")
  Plug 'rizzatti/dash.vim'
endif

call plug#end()

filetype plugin indent on

" Visuals
syntax on                   " enable syntax highlighting
set cmdheight=2             " lower command line one lines high
set mouse=nv                " mouse in normal and visual mode
set modifiable              " needed for vimpager
set bg=dark                 " dark background
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
set backspace=indent,eol,start
set formatoptions=qrn1
set cursorline              " show me the line where the cursor is
set nofoldenable            " don't do any folding for now
set guicursor+=a:blinkon0   " don't blink the cursor please
set winwidth=79             " resize active window to minimally contains 79 chars width
set clipboard=unnamedplus   " enable clipboard when on gui
set completeopt-=preview
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮ " Change listchars
set showbreak=↪             " Change wrap line break
set fillchars=diff:⣿,vert:│ " Change fillchars

" Ripgrep
set grepprg=rg\ --vimgrep
set grepformat=%f:%l:%c:%m

colorscheme nord

" Use normal regular expressions
nnoremap / /\v
vnoremap / /\v

" Bash-like filename completion
set wildmenu
set wildmode=list:longest

" Backup
set undofile
set undodir=~/.config/nvim/tmp/undo//     " undo files
set backupdir=~/.config/nvim/tmp/backup// " backups
set directory=~/.config/nvim/tmp/swap//   " swap files
set backup                                " enable backups
set noswapfile                            " no swapping

" Change mapleader to comma
let mapleader = ","
let maplocalleader = "_"

" Faster scrolling
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

" Shortcuts
inoremap hh <ESC>
map <silent> <Leader>nm :Neomake<cr>
nmap <silent> <Leader>l :set list!<CR>
noremap <silent> <leader>nn :set nu!<cr>
noremap <silent> <Leader>nr :set relativenumber!<cr>
nnoremap <C-l> :nohlsearch<CR><C-l>

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

" Mutt
au BufRead ~/.mutt/tmp/mutt-* set tw=72 formatoptions=tcql

" Make comments italic
highlight Comment cterm=italic

