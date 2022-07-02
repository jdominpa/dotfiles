return require("packer").startup(function()
  use "wbthomason/packer.nvim"

  -- Colorscheme
  use "ishan9299/modus-theme-vim"

  -- Status line
  use {
    "nvim-lualine/lualine.nvim",
    requires = {"kyazdani42/nvim-web-devicons", opt = true}
  }

  -- Utilities
  use "tpope/vim-commentary"
  use "tpope/vim-surround"
  use {"folke/zen-mode.nvim", cmd = {"ZenMode"}}

  -- Telescope
  use {
    "nvim-telescope/telescope.nvim",
    requires = {"nvim-lua/plenary.nvim"}
  }

  -- Treesitter
  use {
    "nvim-treesitter/nvim-treesitter",
    run = ":TSUpdate"
  }

  -- Language plugins
  use {"lervag/vimtex", ft = {"tex", "bib"}}

  -- LSP
  use "neovim/nvim-lspconfig"
  use {
    "williamboman/nvim-lsp-installer",
    requires = {"neovim/nvim-lspconfig"}
  }
  use "onsails/lspkind-nvim"

  -- Completion
  use "hrsh7th/nvim-cmp"
  use "hrsh7th/cmp-buffer"
  use "hrsh7th/cmp-path"
  use "hrsh7th/cmp-nvim-lsp"
  use "hrsh7th/cmp-nvim-lua"
  use "saadparwaiz1/cmp_luasnip"

  -- Snippets
  use "L3MON4D3/LuaSnip"
end)
