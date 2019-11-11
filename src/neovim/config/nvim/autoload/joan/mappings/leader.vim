" Cycle through relativenumber + number, number (only), and no numbering.
function! joan#mappings#leader#cycle_numbering() abort
  if exists('+relativenumber')
    execute {
          \ '00': 'set relativenumber   | set number',
          \ '01': 'set norelativenumber | set number',
          \ '10': 'set norelativenumber | set nonumber',
          \ '11': 'set norelativenumber | set number' }[&number . &relativenumber]
  else
    " No relative numbering, just toggle numbers on and off.
    set number!
  endif
endfunction

" Zap trailing whitespace.
function! joan#mappings#leader#zap() abort
  call joan#functions#substitute('\s\+$', '', '')
endfunction
