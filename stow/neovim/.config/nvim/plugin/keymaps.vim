scriptencoding utf-8

" Make Y behave like the rest of capital actions
nnoremap Y y$

" Center search results when jumping between them
nnoremap n nzzzv
nnoremap N Nzzzv

" Don't move the screen when joining lines
nnoremap J mzJ`z

" Extra undo break points
"inoremap , ,<c-g>u
"inoremap . .<c-g>u

" Move lines
vnoremap <M-j> :m '>+1<CR>gv=gv
vnoremap <M-k> :m '<-2<CR>gv=gv
inoremap <M-j> <Esc>:m .+1<CR>==gi
inoremap <M-k> <Esc>:m .-2<CR>==gi

" Remove whitespace
nnoremap <leader>ws :%s/\s\+$//<CR>

" Better window navigation
nnoremap <C-h> <C-w><C-h>
nnoremap <C-j> <C-w><C-j>
nnoremap <C-k> <C-w><C-k>
nnoremap <C-l> <C-w><C-l>
