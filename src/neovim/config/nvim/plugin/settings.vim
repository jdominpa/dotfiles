
set noswapfile                  " Don't create swap files
set nobackup			" No backups
set nowritebackup		" No backups

if has('linebreak')
    set linebreak		" Wrap long lines at characters in 'breakat'
endif

set textwidth=80		" Hard wrap at 80 columns
set breakindent			" Don't break the indentation when wrapping lines
set number                      " Show line numbers
set relativenumber              " Show relative numbers
set cursorline			" Highlight current line

if has('windows')
    set splitbelow              " Split window below
endif

if has('vertsplit')
    set splitright              " Split window right
endif

set ignorecase			" Case insensitive search
set smartcase			" Smart search

set softtabstop=4               " Tabs are spaces
set shiftwidth=4                " Spaces for indenting
set smartindent			" Smart indentation for c and stuff

set foldcolumn=0		" Column to show folds
set foldenable			" Enable folding
set foldlevel=0			" Close all folds by default
set foldmethod=marker		" Syntax are used to specify folds
set foldminlines=0		" Allow folding single lines
set foldnestmax=5		" Set max fold nesting level

set shortmess+=c		" Don't give completion messages
