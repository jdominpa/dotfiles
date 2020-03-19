if ! has('clipboard')
  finish
endif

vnoremap <C-c> "*y :let @+=@*<CR>
nnoremap <C-p> "+p
