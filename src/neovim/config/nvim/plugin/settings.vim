scriptencoding utf-8

set noswapfile                  " Don't create swap files
set nobackup			" No backups
set nowritebackup		" No backups

if has('linebreak')
  set linebreak		" Wrap long lines at characters in 'breakat'
  let &showbreak='↳ '         " DOWNWARDS ARROW WITH TIP RIGHTWARDS (U+21B3, UTF-8: E2 86 B3)
endif

set textwidth=80		" Hard wrap at 80 columns
set breakindent			" Don't break the indentation when wrapping lines
set number                      " Show line numbers
set relativenumber              " Show relative numbers
set cursorline			" Highlight current line

set list			" Show whitespaces
set listchars=nbsp:⦸            " CIRCLED REVERSE SOLIDUS (U+29B8, UTF-8: E2 A6 B8)
set listchars+=tab:>┅           " BOX DRAWINGS HEAVY TRIPLE DASH HORIZONTAL (U+2505, UTF-8: E2 94 85)
set listchars+=extends:»        " RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK (U+00BB, UTF-8: C2 BB)
set listchars+=precedes:«       " LEFT-POINTING DOUBLE ANGLE QUOTATION MARK (U+00AB, UTF-8: C2 AB)
set listchars+=trail:•          " BULLET (U+2022, UTF-8: E2 80 A2)

if has('windows')
  set splitbelow              " Split window below
endif

if has('vertsplit')
  set splitright              " Split window right
endif

set ignorecase			" Case insensitive search
set smartcase			" Smart search

set tabstop=4
set softtabstop=4               " Tabs are spaces
set shiftwidth=4                " Spaces for indenting
set smartindent			" Smart indentation for c and stuff

if has('folding')
  if has('windows')
    set fillchars=diff:∙               " BULLET OPERATOR (U+2219, UTF-8: E2 88 99)
    set fillchars+=fold:·              " MIDDLE DOT (U+00B7, UTF-8: C2 B7)
    set fillchars+=vert:┃              " BOX DRAWINGS HEAVY VERTICAL (U+2503, UTF-8: E2 94 83)
  endif

  if has('nvim-0.3.1')
    set fillchars+=eob:\              " Suppress ~ at EndOfBuffer
  endif

  set foldmethod=indent               " Not as cool as syntax, but faster
  set foldlevelstart=99               " Start unfolded
  set foldminlines=0		      " Allow folding single lines
  set foldtext=joan#settings#foldtext()
endif

set hidden                      " Allows you to hide buffers with unsaved changes without being prompted
set shortmess+=c		" Don't give completion messages

if has('wildignore')
  set wildignore+=*.o           " Patterns to ignore during file-navigation
endif
