if !has('nvim')
  finish
endif

if exists("&termguicolors") && exists("&winblend")
  set termguicolors
  set winblend=0
  set wildoptions=pum
  set pumblend=20
  set background=dark
  colorscheme modus-vivendi
endif
