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
Plug 'w0rp/ale'
  let g:ale_lint_on_save = 1
  let g:ale_lint_on_enter = 0
  let g:ale_lint_on_text_changed = 0
  let g:ale_rust_cargo_check_all_targets = 1
Plug 'tpope/vim-eunuch'                                  " unix helper commands
Plug 'tpope/vim-repeat'                                  " make the . command available to more plugins
Plug 'tpope/vim-unimpaired'                              " bracket mappings for easy jumping
Plug 'tpope/vim-speeddating'                             " easily increment numbers and dates
Plug 'jiangmiao/auto-pairs'                              " close pairing symbols automatically
Plug 'airblade/vim-gitgutter'                            " show git changes in the gutter
Plug 'rking/ag.vim'                                      " silver searcher
Plug 'tpope/vim-commentary'                              " comment mappings
Plug 'tpope/vim-surround'                                " surround commands
Plug 'godlygeek/tabular'                                 " easy indenting
Plug 'mbbill/undotree'                                   " easy undoing
  nnoremap <silent> <Leader>ut :UndotreeToggle<cr>
Plug 'Shougo/echodoc.vim'                                " show function signature
  let g:echodoc#enable_at_startup = 1
Plug 'Shougo/vimproc', {'do': 'make'}                    " command execution
Plug 'junegunn/fzf.vim'                                  " quick file/buffer browsing
Plug 'airblade/vim-rooter'                               " automatically set the root path
Plug 'majutsushi/tagbar'                                 " sidebar to jump to regions
  map <C-t> :TagbarToggle<CR>
  
" Colors
Plug 'cocopon/iceberg.vim'

Plug 'vim-airline/vim-airline'
    let g:airline_powerline_fonts = 0
    let g:airline_left_sep = ''
    let g:airline_right_sep = ''
    let g:airline#extensions#whitespace#enabled = 0
    let g:airline#extensions#ale#enabled = 1

" Filetype support
Plug 'othree/html5.vim', { 'for': 'html' }
Plug 'hail2u/vim-css3-syntax'
Plug 'cakebaker/scss-syntax.vim', { 'for': 'scss' }
Plug 'chr4/nginx.vim', { 'for': 'nginx' }
Plug 'aliva/vim-fish', { 'for': 'fish' }
Plug 'pearofducks/ansible-vim'

" Rust
Plug 'rust-lang/rust.vim', { 'for': 'rust' }
  let g:rustfmt_autosave = 1
  let g:rustfmt_command = 'rustfmt +nightly'
Plug 'cespare/vim-toml', { 'for': 'toml' }

" Go
Plug 'fatih/vim-go', { 'for': 'go' }
  let g:go_play_open_browser = 0
  let g:go_fmt_fail_silently = 1
  let g:go_fmt_command = "goimports"
  au BufNewFile,BufRead *.go setlocal noet ts=4 sw=4 sts=4

" Elm
Plug 'elmcast/elm-vim', { 'for': 'elm' }
  let g:elm_format_autosave = 1

" Elixir
Plug 'elixir-editors/vim-elixir'
Plug 'slashmili/alchemist.vim'

" Autocomplete and language server
Plug 'neoclide/coc.nvim', {'do': { -> coc#util#install()}}
  nmap <silent> gd <Plug>(coc-definition)
  nmap <silent> gy <Plug>(coc-type-definition)
  nmap <silent> gi <Plug>(coc-implementation)
  nmap <silent> gr <Plug>(coc-references)
  " Remap for rename current word
  nmap <leader>rn <Plug>(coc-rename)

  " Use K to show documentation in preview window
  nnoremap <silent> K :call <SID>show_documentation()<CR>

  function! s:show_documentation()
    if (index(['vim','help'], &filetype) >= 0)
      execute 'h '.expand('<cword>')
    else
      call CocAction('doHover')
    endif
  endfunction

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
let mapleader = ","
let maplocalleader = "_"

" Use normal regular expressions
nnoremap / /\v
vnoremap / /\v

" FZF
set rtp+=/usr/local/opt/fzf
set rtp+=/usr/share/vim/vimfiles
set rtp+=~/.fzf
nmap <C-p> :Files<cr>

let g:fzf_action = {
      \ 'ctrl-t': 'tab split',
      \ 'ctrl-i': 'split',
      \ 'ctrl-s': 'vsplit' }
let g:fzf_layout = { 'down': '~20%' }

let g:rg_command = '
      \ rg --column --line-number --no-heading --fixed-strings --ignore-case --no-ignore --hidden --follow --color "always"
      \ -g "*.{js,json,php,md,styl,pug,jade,html,config,py,cpp,c,go,hs,rb,conf,fa,lst}"
      \ -g "!{.config,.git,node_modules,vendor,build,yarn.lock,*.sty,*.bst,*.coffee,dist}/*" '

command! -bang -nargs=* F call fzf#vim#grep(g:rg_command .shellescape(<q-args>), 1, <bang>0)

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
