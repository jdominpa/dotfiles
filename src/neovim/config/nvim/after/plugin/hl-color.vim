if !has('nvim')
  finish
endif

function s:RemoveBg(group)
  let l:highlight=filter(luaeval("require'wincent.pinnacle'.dump(_A)", a:group), 'v:key != "bg"')
  execute 'highlight! clear ' . a:group
  execute 'highlight! ' . a:group . ' ' . luaeval("require'wincent.pinnacle'.highlight(_A)", l:highlight)
endfunction

function s:CheckHighlightColors()
  execute 'highlight Comment ' . luaeval("require'wincent.pinnacle'.italicize('Comment')")

  " Hide (or at least make less obvious) the EndOfBuffer region
  highlight! EndOfBuffer ctermbg=bg ctermfg=bg guibg=bg guifg=bg

  highlight clear CursorLineNr
  execute 'highlight CursorLineNr ' . luaeval("require'wincent.pinnacle'.extract_highlight('DiffText')")

  highlight clear Pmenu
  highlight link Pmenu Visual

  highlight clear DiffDelete
  highlight link DiffDelete Conceal
  highlight clear VertSplit
  highlight link VertSplit LineNr

  " Resolve clashes with ColorColumn.
  " Instead of linking to Normal (which has a higher priority, link to nothing).
  highlight link vimUserFunc NONE

  " For Git commits, suppress the background of these groups:
  for l:group in ['DiffAdded', 'DiffFile', 'DiffNewFile', 'DiffLine', 'DiffRemoved']
    call s:RemoveBg(l:group)
  endfor

  " More subtle highlighting during merge conflict resolution.
  highlight clear DiffAdd
  highlight clear DiffChange
  highlight clear DiffText

  let l:highlight=luaeval("require'wincent.pinnacle'.italicize('ModeMsg')")
  execute 'highlight User8 ' . l:highlight

  " Allow for overrides:
  " - `statusline.vim` will re-set User1, User2 etc.
  " - `after/plugin/loupe.vim` will override Search.
  doautocmd ColorScheme
endfunction

if v:progname !=# 'vi'
  if has('autocmd')
    augroup JdominpaAutocolor
      autocmd!
      autocmd FocusGained * call s:CheckHighlightColors()
    augroup END
  endif

  call s:CheckHighlightColors()
endif
