" Use <Tab> and <S-Tab> to navigate through popup menu
"inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
"inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" Add <Up> and <Down> to popup menu keybindings
inoremap <expr> <Up>   pumvisible() ? "\<C-p>" : "\<Up>"
inoremap <expr> <Down> pumvisible() ? "\<C-n>" : "\<Down>"

" Bind C-y to confirm completion
let g:completion_confirm_key = "\<C-y>"

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
