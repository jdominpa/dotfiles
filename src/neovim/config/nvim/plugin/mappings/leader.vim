" Leader mappings

" Buffer navigation
nnoremap <Leader><Leader> <C-^>
nnoremap <Leader>ls :buffers<CR>

" <Leader>r -- Cycle through relativenumber + number, number (only), and no
" numbering (mnemonic: relative).
nnoremap <silent> <Leader>r :call joan#mappings#leader#cycle_numbering()<CR>

" <Leader>zz -- Zap trailing whitespace in the current buffer.
"
"        As this one is somewhat destructive and relatively close to the
"        oft-used <leader>a mapping, make this one a double key-stroke.
nnoremap <silent> <Leader>zz :call joan#mappings#leader#zap()<CR>
