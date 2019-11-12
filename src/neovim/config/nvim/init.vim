" Generic settings ------------------------------------------------------------------

" Leader key
let mapleader = "\<Space>"


" Plugins ---------------------------------------------------------------------------

" Download and install vim-plug automatically
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  echo "Downloading vim-plug and installing plugins"
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Load plugins
call plug#begin('~/.config/nvim/plugged')

" Utility
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'
Plug 'scrooloose/nerdtree'

" Autocompletion
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Snippets
Plug 'sirver/ultisnips'

" Fzf
Plug '/usr/local/opt/fzf'

" Theme and style
Plug 'ryanoasis/vim-devicons'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'chriskempson/base16-vim'

call plug#end()


" Load init.vim.local settings ------------------------------------------------------

if filereadable(glob("~/.config/nvim/init.vim.local"))
  source ~/.config/nvim/init.vim.local
endif
