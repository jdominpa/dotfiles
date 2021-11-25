" Generic settings ------------------------------------------------------------------

" Leader key
let mapleader = "\<Space>"
let maplocalleader = ","


" Plugins ---------------------------------------------------------------------------

" Load plugins
call plug#begin('~/.config/nvim/plugged')

Plug 'chriskempson/base16-vim'            " color scheme
Plug 'ishan9299/modus-theme-vim'          " color scheme
Plug 'junegunn/goyo.vim'                  " distraction-free writing in vim
Plug 'lervag/vimtex'                      " extra functionality for editing LaTeX documents
Plug 'neovim/nvim-lspconfig'              " neovim built-in LSP configuration
Plug 'nvim-lua/completion-nvim'           " neovim built-in LSP completion
Plug 'nvim-lua/popup.nvim'                " -
Plug 'nvim-lua/plenary.nvim'              " fuzzy finder
Plug 'nvim-telescope/telescope.nvim'      " -
Plug 'SirVer/ultisnips'                   " snippets
Plug 'tpope/vim-commentary'               " functionality to comment lines
Plug 'tpope/vim-surround'                 " functionality to edit surrounding characters
Plug 'tweekmonster/startuptime.vim'       " benchmarking startup
Plug 'vim-airline/vim-airline'            " statusbar configuration
Plug 'vim-airline/vim-airline-themes'     " statusbar configuration

call plug#end()


" Load init.vim.local settings ------------------------------------------------------

if filereadable(glob("~/.config/nvim/init.vim.local"))
  source ~/.config/nvim/init.vim.local
endif
