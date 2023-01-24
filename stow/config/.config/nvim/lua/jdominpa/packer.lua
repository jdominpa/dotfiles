return require("packer").startup(function()
  use "wbthomason/packer.nvim"

  -- Colorscheme
  use { "catppuccin/nvim", as = "catppuccin" }

  -- Status line
  use {
    "nvim-lualine/lualine.nvim",
    requires = {"kyazdani42/nvim-web-devicons", opt = true}
  }

  -- Utilities
  use "numToStr/Comment.nvim"
  use "tpope/vim-surround"
  use "folke/zen-mode.nvim"

  -- Telescope
  use {
    "nvim-telescope/telescope.nvim",
    requires = {"nvim-lua/plenary.nvim"}
  }

  -- Treesitter
  use {
    "nvim-treesitter/nvim-treesitter",
    run = ":TSUpdate",
    requires = {
      "nvim-treesitter/nvim-treesitter-textobjects",
    },
  }

  -- Language plugins
  use { "lervag/vimtex", ft = {"tex", "bib"} }

  -- LSP
  use {
    "neovim/nvim-lspconfig",
    requires = {
      "j-hui/fidget.nvim",
      "onsails/lspkind-nvim",
    },
  }

  -- Completion
  use {
    "hrsh7th/nvim-cmp",
    requires = {
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-nvim-lua",
      "L3MON4D3/LuaSnip",
      "saadparwaiz1/cmp_luasnip",
    },
  }

  -- Vim-be-good
  use "ThePrimeagen/vim-be-good"

end)
