" Quit if fzf isn't loaded
if !exists(':FZF')
    finish
endif

nnoremap <Leader>f :FZF<CR>
nnoremap <Leader>FH :FZF ~<CR>
nnoremap <Leader>FF :FZF
