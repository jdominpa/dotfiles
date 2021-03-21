" Visual mode mappings.

" Move VISUAL LINE selection within buffer.
xnoremap <silent> K :call jdominpa#mappings#visual#move_up()<CR>
xnoremap <silent> J :call jdominpa#mappings#visual#move_down()<CR>
