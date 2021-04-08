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
Plug 'airblade/vim-gitgutter'                            " show git changes in the gutter
Plug 'tpope/vim-commentary'                              " comment mappings
Plug 'tpope/vim-surround'                                " surround commands
Plug 'airblade/vim-rooter'                               " automatically set the root path

" Browsing with FZF
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
  nmap <C-p> :Files<cr>
  let g:fzf_action = {
      \ 'ctrl-t': 'tab split',
      \ 'ctrl-i': 'split',
      \ 'ctrl-s': 'vsplit' }

  let g:fzf_layout = { 'down': '20%' }

  let g:rg_command = '
      \ rg --column --line-number --no-heading --fixed-strings --ignore-case --no-ignore --hidden --follow --color "always"
      \ -g "*.{js,json,php,md,styl,pug,jade,html,config,py,cpp,c,go,hs,rb,conf,fa,lst}"
      \ -g "!{.config,.git,node_modules,vendor,build,yarn.lock,*.sty,*.bst,*.coffee,dist}/*" '

command! -bang -nargs=* F call fzf#vim#grep(g:rg_command .shellescape(<q-args>), 1, <bang>0)
  
" Colors
Plug 'cocopon/iceberg.vim'
Plug 'https://gitlab.com/yorickpeterse/vim-paper.git'

Plug 'vim-airline/vim-airline'
   let g:airline_powerline_fonts = 0
   let g:airline_left_sep = ''
   let g:airline_right_sep = ''
   let g:airline#extensions#whitespace#enabled = 0
   let g:airline#extensions#ale#enabled = 1
Plug 'vim-airline/vim-airline-themes'

" Language Server
source $HOME/.config/nvim/coc.vimrc

" Filetype support
Plug 'sheerun/vim-polyglot'
  autocmd FileType elixir setlocal formatprg=mix\ format\ -

Plug 'Olical/conjure', {'tag': 'v4.17.0'}

call plug#end()

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
set number                  " Show line number
set backspace=indent,eol,start
set formatoptions=qrn1
set nocursorline            " show me the line where the cursor is
set nofoldenable            " don't do any folding for now
set guicursor+=a:blinkon0   " don't blink the cursor please
set winwidth=80             " resize active window to minimally contains 80 chars width
set clipboard=unnamedplus   " enable clipboard when on gui
set listchars=nbsp:¬,extends:»,precedes:«,trail:•
set showbreak=↪             " change wrap line break
set fillchars=diff:⣿,vert:│ " change fillchars
set splitright              " new windows are on the right
set splitbelow              " new windows are below the current one
set termguicolors
colorscheme iceberg

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
set undodir=~/.config/nvim/tmp/undo//     " undo files
set backupdir=~/.config/nvim/tmp/backup// " backups
set directory=~/.config/nvim/tmp/swap//   " swap files
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

" Mutt
au BufRead ~/.mutt/tmp/mutt-* set tw=72 formatoptions=tcql

" Make comments italic
hi Comment gui=italic cterm=italic
hi htmlArg gui=italic cterm=italic
