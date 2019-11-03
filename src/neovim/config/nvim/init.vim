" vim:fdm=marker

" Generic settings ------------------------------------------------------------------

" Leader key {{{
let mapleader=","
" }}}

" Some junk {{{
set noswapfile                  " Don't create swap files
set nobackup			" No backups
set nowritebackup		" No backups
set nowrap                      " Don't wrap long lines
set breakindent			" Don't break the indentation when wrapping lines
set number                      " Show line numbers
set relativenumber              " Show relative numbers
set splitbelow                  " Split window below
set splitright                  " Split window right
set ignorecase			" Case insensitive search
set smartcase			" Smart search
set softtabstop=4               " Tabs are spaces
set shiftwidth=4                " Spaces for indenting
set smartindent			" Smart indentation for c and stuff
set foldcolumn=0		" Column to show folds
set foldenable			" Enable folding
set foldlevel=0			" Close all folds by default
set foldmethod=marker		" Syntax are used to specify folds
set foldminlines=0		" Allow folding single lines
set foldnestmax=5		" Set max fold nesting level
set shortmess+=c		" Don't give completion messages
" }}}

" Remove whitespace on save {{{
autocmd BufWritePre * %s/\s\+$//e
" }}}


" Mappings --------------------------------------------------------------------------

" Window navigation remaps {{{
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
" }}}

" Buffers {{{
augroup buffer_control
  autocmd!

  " Prompt for buffer to select (,bs) {{{
  nnoremap <leader>bs :CtrlPBuffer<CR>
  " }}}

  " Buffer navigation (,,) (gb) (gB) (,ls) {{{
  map <Leader>, <C-^>
  map <Leader>ls :buffers<CR>
  map gb :bnext<CR>
  map gB :bprev<CR>
  " }}}
augroup END
" }}}

" System clipboard {{{
vnoremap <C-c> "*y :let @+=@*<CR>
nnoremap <C-p> "+p
" }}}

" Remap W to w {{{
command! W write
" }}}

" Yank from cursor to end of line {{{
nnoremap Y y$
" }}}


" Plugins ---------------------------------------------------------------------------

" Download and install vim-plug automatically {{{
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  echo "Downloading vim-plug and installing plugins"
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
" }}}

" Load plugins {{{
call plug#begin('~/.config/nvim/plugged')

" Utility
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'
Plug 'scrooloose/nerdtree'

" Autocompletion
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Fzf
Plug '/usr/local/opt/fzf'

" Theme and style
Plug 'ryanoasis/vim-devicons'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'morhetz/gruvbox'

call plug#end()
" }}}


" Plugin configuration --------------------------------------------------------------

" Colorscheme {{{
if &t_Co > 2 || has('gui_running')
    colorscheme gruvbox
endif
" }}}

" Vim-airline {{{
let g:airline#extensions#tabline#enabled=1
let g:airline_powerline_fonts=1
let g:airline_theme='gruvbox'
" }}}

" NERDTree {{{
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
nnoremap <Leader>t :NERDTreeToggle<CR>
" }}}

" Fzf {{{
nnoremap <Leader>f :FZF<CR>
" }}}

" Coc.nvim {{{

" Coc.nvim config command {{{
" Function to abreviate a command
function! SetupCommandAbbrs(from, to)
  exec 'cnoreabbrev <expr> '.a:from
        \ .' ((getcmdtype() ==# ":" && getcmdline() ==# "'.a:from.'")'
        \ .'? ("'.a:to.'") : ("'.a:from.'"))'
endfunction

" Use C to open coc config
call SetupCommandAbbrs('C', 'CocConfig')
" }}}

" Tab completion {{{
" Use tab for everything
inoremap <silent><expr> <TAB>
      \ pumvisible() ? coc#_select_confirm() :
      \ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

let g:coc_snippet_next = '<tab>'

" Use tab to scroll through suggestions
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" Use cr to confirm completion
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

" Close preview window when completion is done
autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif
" }}}

" Gotos {{{
" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
" }}}

" Diagnose {{{
" Show all diagnostics
nnoremap <silent> <Leader>d  :<C-u>CocList diagnostics<cr>
" }}}

" Extensions {{{
let g:coc_global_extensions=["coc-json", "coc-ccls"]
" }}}
" }}}


" Filetypes -------------------------------------------------------------------------

" C {{{
augroup filetype_c
  autocmd!

  " Highlight Custom C Types {{{
  autocmd BufRead,BufNewFile *.[ch] let fname = expand('<afile>:p:h') . '/types.vim'
  autocmd BufRead,BufNewFile *.[ch] if filereadable(fname)
  autocmd BufRead,BufNewFile *.[ch]   exe 'so ' . fname
  autocmd BufRead,BufNewFile *.[ch] endif
  " }}}
augroup END
" }}}


" Load init.vim.local settings ------------------------------------------------------

" {{{
if filereadable(glob("~/.config/nvim/init.vim.local"))
    source ~/.config/nvim/init.vim.local
endif
" }}}
