" Quit if fzf isn't loaded
if !exists(':FZF')
    finish
endif

nnoremap <Leader>f :FzfFiles<CR>
nnoremap <Leader>FH :FzfFiles ~<CR>
nnoremap <Leader>FF :FzfFiles 
nnoremap <Leader>b :FzfBuffers<CR>
