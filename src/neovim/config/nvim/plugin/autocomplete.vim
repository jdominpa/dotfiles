" Navigation in completion menu
inoremap <expr> <C-j> pumvisible() ? "\<C-n>" : "\<C-j>"
inoremap <expr> <Down> pumvisible() ? "\<C-n>" : "\<Down>"
inoremap <expr> <C-k> pumvisible() ? "\<C-p>" : "\<C-k>"
inoremap <expr> <Up>   pumvisible() ? "\<C-p>" : "\<Up>"

" Bind C-y to confirm completion
let g:completion_confirm_key = "<C-Space>"

" Set completeopt to have a better completion experience
set completeopt=menuone,noinsert,noselect

" Completion matching settings
let g:completion_matching_strategy_list = ['exact', 'substring', 'fuzzy']
let g:completion_matching_smart_case = 1

" Enable snippet support
let g:completion_enable_snippet = 'UltiSnips'

" Triggers for ultisnips
let g:UltiSnipsExpandTrigger="<Tab>"
let g:UltiSnipsJumpForwardTrigger="<Tab>"
let g:UltiSnipsJumpBackwardTrigger="<S-Tab>"

" Ultisnips directory
let g:UltiSnipsSnippetDirectories = ['ultisnips']
