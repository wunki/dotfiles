set cmdheight=2             " lower command line two lines high
set modifiable              " needed for vimpager
set bg=dark                 " dark background
syntax on                   " enable syntax highlighting
filetype plugin indent on

call plug#begin('~/.config/nvim/plugged')

" Essentials
Plug 'tpope/vim-eunuch'                                  " unix helper commands
Plug 'tpope/vim-repeat'                                  " make the . command available to more plugins
Plug 'tpope/vim-unimpaired'                              " bracket mappings for easy jumping
Plug 'tpope/vim-obsession'                               " better vim sessions
Plug 'rking/ag.vim'                                      " silver searcher
Plug 'commentary.vim'                                    " comment mappings
Plug 'surround.vim'                                      " surround commands
Plug 'godlygeek/tabular'                                 " easy indenting
Plug 'Lokaltog/vim-easymotion'                           " move by selecting a letter
Plug 'scrooloose/syntastic'                              " syntax checker
Plug 'mbbill/undotree'                                   " easy undoing
Plug 'jiangmiao/auto-pairs'                              " pair parenthesis, brackend and quotes
Plug 'dhruvasagar/vim-table-mode', { 'for': 'markdown' } " table creation in markdown
Plug 'kien/ctrlp.vim'                                    " fast file switching
Plug 'majutsushi/tagbar'                                 " sidebar to jump to regions
    map <C-t> :Tagbar<CR>
Plug 'Shougo/vimproc', {'do': 'gmake'}                   " command execution
Plug 'Chiel92/vim-autoformat'                            " autoformat on save
    let g:formatdef_rustfmt = '"rustfmt"'
    let g:formatters_rust = ['rustfmt']

" Autocomplete
Plug 'Shougo/deoplete.nvim'
    let g:acp_enableAtStartup = 0
    let g:deoplete#enable_at_startup = 1
    let g:deoplete#enable_ignore_case = 'ignorecase'
    let g:deoplete#sources#syntax#min_keyword_length = 3

" Looks
Plug 'morhetz/gruvbox'
Plug 'itchyny/lightline.vim'
    let g:lightline = {
                \ 'colorscheme': 'gruvbox',
                \ }

" HTML
Plug 'Glench/Vim-Jinja2-Syntax', { 'for': ['html', 'sls'] }
Plug 'othree/html5.vim', { 'for': 'html' }

" Go
Plug 'benmills/vim-golang-alternate', { 'for': 'go' }
Plug 'fatih/vim-go', { 'for': 'go' }
    au BufNewFile,BufRead *.go setlocal noet ts=4 sw=4 sts=4
    let g:go_fmt_command = "goimports"
    let g:go_auto_type_info = 1

    au FileType go nmap <leader>r <Plug>(go-run)
    au FileType go nmap <leader>b <Plug>(go-build)
    au FileType go nmap <leader>t <Plug>(go-test)
    au FileType go nmap <Leader>i <Plug>(go-info)
    au FileType go nmap <Leader>d <Plug>(go-doc)
    au FileType go nmap <Leader>gv <Plug>(go-doc-vertical)
    au FileType go nmap <Leader>gb <Plug>(go-doc-browser)
    au FileType go nmap gd <Plug>(go-def)

" Rust
Plug 'rust-lang/rust.vim', { 'for': 'rust' }
Plug 'phildawes/racer', { 'do': 'cargo build --release', 'for': 'rust' }
Plug 'cespare/vim-toml', { 'for': 'toml' }

" Fish
Plug 'aliva/vim-fish', { 'for': 'fish' }

" Servers
Plug 'saltstack/salt-vim', { 'for': 'sls' }
Plug 'pearofducks/ansible-vim'

call plug#end()

" Visuals
set mouse=a
colorscheme gruvbox

set showmatch               " show matching brackets (), {}' []
set showcmd                 " show command
set modeline                " enable modelines
set ignorecase              " case-insensitive search
set smartcase               " upper-case sensitive search
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
set number                  " show linenumbers
set undofile                " create undofiles
set formatoptions=qrn1
set nocursorline            " don't show me the line where the cursor is
set nofoldenable            " don't do any folding for now
set guicursor+=a:blinkon0   " don't blink the cursor please
set winwidth=79             " resize active window to minimally contains 79 chars width

" ignore these files in netrw
let g:netrw_list_hide= '.*\.swp$,.*\~$,.*\.pyc$'

" Use normal regular expressions
nnoremap / /\v
vnoremap / /\v

" Bash-like filename completion
set wildmenu
set wildmode=list:longest

" Ignore these things
set wildignore+=.hg,.git,.svn                    " Version control
set wildignore+=*.aux,*.out,*.toc                " LaTeX intermediate files
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg   " binary images
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest " compiled object files
set wildignore+=*.spl                            " compiled spelling word lists
set wildignore+=*.sw?                            " Vim swap files
set wildignore+=*.DS_Store                       " OSX
set wildignore+=*.pyc                            " Python byte code
set wildignore+=*.orig                           " Merge resolution files
set wildignore+=*.beam                           " Erlang object code
set wildignore+=classes                          " java/clojure classes
set wildignore+=cabal-dev                        " cabal
set wildignore+=dist                             " cabal distrubition
set wildignore+=*.test                           " ignore Go's tests files
set wildignore+=*/Godeps/*                       " ignore godeps directory
set wildignore+=*/node_modules/*                 " ignore node.js packages

" jumping around
set tags=tags;/,codex.tags;/

" only show the cursorline on the active window
augroup cline
    au!
    au WinLeave,InsertEnter * set nocursorline
    au WinEnter,InsertLeave * set cursorline
augroup END

" backup settings
set undodir=~/.vim/tmp/undo//     " undo files
set backupdir=~/.vim/tmp/backup// " backups
set directory=~/.vim/tmp/swap//   " swap files
set backup                        " enable backups
set noswapfile                    " no swapping

" point neovim to python
let g:python_host_prog='/usr/sbin/python'

" Faster scrolling
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

" Whitespace
set nowrap
set tabstop=4                     " tab width is 4 spaces
set shiftwidth=4                  " indent is also with 4 spaces
set expandtab                     " expand tabs to spaces

" Change mapleader to comma
let mapleader = ","
let maplocalleader = "_"

" Toggle whitespace invisibles
nmap <leader>l :set list!<CR>

" Go settings
" Rust settings
let g:racer_cmd = "/home/wunki/.vim/plugged/racer/target/release/cargo"
if has('mac')
    let $RUST_SRC_PATH="/Users/wunki/Rust/rust/src"
else
    let $RUST_SRC_PATH="/home/wunki/rust/rust/src"
end

" Mutt settings
au BufRead ~/.mutt/tmp/mutt-* set tw=72 formatoptions=tcql

" JSON
au! BufRead,BufNewFile *.json setfiletype json

" Quickly escape to normal mode
inoremap hh <ESC>

" Quick edit
nnoremap <leader>ev :vsplit $MYVIMRC<cr>

" Line numbers
noremap <f3> :set nu!<cr>
noremap <f4> :set relativenumber!<cr>

" Go up/down a display line, instead of actual line
nnoremap j gj
nnoremap k gk

" Formatting
nnoremap Q gqip
vnoremap Q gq

" Undo tree
nnoremap <F5> :UndotreeToggle<cr>

" Ctrlp
let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
let g:ctrlp_working_path_mode = 0
nnoremap <silent> <C-b> :CtrlPBuffer<cr>
nnoremap <silent> <C-B> :CtrlPBookmarkDir<cr>

" Commentary
nmap <leader>c <Plug>CommentaryLine
xmap <leader>c <Plug>Commentary

augroup plugin_commentary
    au!
    au FileType htmldjango setlocal commentstring={#\ %s\ #}
    au FileType clojurescript setlocal commentstring=;\ %s
    au FileType lisp setlocal commentstring=;\ %s
    au FileType rust setlocal commentstring=//\ %s
    au FileType cf3 setlocal commentstring=\#\ %s
augroup END

" Completion
set complete=.,w,b,u,t
set completeopt=menu,menuone

" Syntastic
let g:syntastic_echo_current_error = 0
let g:syntastic_enable_highlighting = 0
let g:syntastic_always_populate_loc_list = 1

" Files
autocmd BufRead,BufNewFile /etc/nginx/sites-*/* setfiletype conf

" Tagbar
let g:tagbar_type_markdown = {
            \ 'ctagstype' : 'markdown',
            \ 'kinds' : [
            \ 'h:headings',
            \ 'l:links',
            \ 'i:images'
            \],
            \ "sort" : 0
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
            \  ]
            \}
