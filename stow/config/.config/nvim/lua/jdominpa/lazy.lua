local path = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(path) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    path,
  })
end
vim.opt.rtp:prepend(path)

require("lazy").setup({
  -- Colorscheme
  {
    "catppuccin/nvim",
    lazy = false,
    priority = 1000,
    config = function()
      require("catppuccin").setup({
        flavour = "mocha",
        no_italic = true,
      })
      vim.cmd([[colorscheme catppuccin]])
    end,
  },

  -- Status line
  { "kyazdani42/nvim-web-devicons", lazy = true },
  "nvim-lualine/lualine.nvim",

  -- Utilities
  {
    "numToStr/Comment.nvim",
    event = "VeryLazy",
  },
  "tpope/vim-surround",
  {
    "folke/zen-mode.nvim",
    cmd = "ZenMode",
  },

  -- Telescope
  {
    "nvim-telescope/telescope.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    cmd = "Telescope",
  },

  -- Treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    event = "VeryLazy",
    dependencies = { "nvim-treesitter/nvim-treesitter-textobjects" },
  },

  -- Latex
  {
    "lervag/vimtex",
    ft = { "tex", "bib" },
  },

  -- LSP
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "j-hui/fidget.nvim",
      "onsails/lspkind-nvim",
    },
  },

  -- Completion
  {
    "hrsh7th/nvim-cmp",
    event = "InsertEnter",
    dependencies = {
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-nvim-lua",
      "L3MON4D3/LuaSnip",
      "saadparwaiz1/cmp_luasnip",
    },
  },
})
