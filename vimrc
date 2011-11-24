"""""""""
" GENERAL
"""""""""
syntax on
set number
set mouse=a

" make pretty
if &term =~ "rxvt-unicode"
    set t_Co=256
    colorscheme ir_black
endif

" indenting
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab

" scroll the viewport faster
nnoremap <C-e> 5<C-e>
nnoremap <C-y> 5<C-y>

" save
map <C-s> :w<CR>
imap <C-s> <ESC>:w<CR>a

" toggle paste mode
nnoremap <F2> :set invpaste paste?<CR>
imap <F2> <C-O><F2>
set pastetoggle=<F2>

" toggle spellchecking
map <F7> :setlocal spell! spelllang=en_us<CR>
imap <F7> <C-o>:setlocal spell! spelllang=en_us<CR>

" switching between windows
map <C-j> <C-W>
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" searching
set incsearch
set ignorecase
set smartcase
set hlsearch
map <silent> \ :nohl<CR>

" emacs keys
imap <C-E> <End>
imap <C-A> <Home>
imap <C-T> <Esc>v^xi
map  <C-K> D
map! <C-K> <Esc><Right>Da

" yank to end of line
nnoremap Y y$

" braces autocomplet
inoremap {<CR>  {<CR>}<Esc>O

" disable auto comment
au FileType * setl fo-=cro

"""""""""
" PLUGINS
"""""""""
filetype plugin indent on

" set supertab mode
let g:SuperTabDefaultCompletionType = "<c-n>"

" toggle taglist
let Tlist_Exit_OnlyWindow = 1
let Tlist_Use_Right_Window = 1
nmap <silent> <F4> :TlistToggle<CR>

" toggle nerdtree
let g:NERDTreeQuitOnOpen = 1
nmap <silent> <F3> :NERDTreeToggle<CR>

" toggle bufexplorer
nmap <silent> <F5> :BufExplorer<CR>
