return {
  -- Colorscheme
  {
    "catppuccin/nvim",
    config = function()
      require("plugins.config.catppuccin")
    end,
  },

  -- Status line
  {
    "nvim-lualine/lualine.nvim",
    config = function()
      require("plugins.config.lualine")
    end,
    dependencies = { "kyazdani42/nvim-web-devicons" },
  },

  -- Utilities
  {
    "tpope/vim-surround",
    event = "VeryLazy",
  },
  {
    "numToStr/Comment.nvim",
    event = "VeryLazy",
    config = function()
      require("plugins.config.comments")
    end,
  },
  {
    "folke/zen-mode.nvim",
    cmd = "ZenMode",
    config = function()
      require("plugins.config.zen")
    end,
  },

  -- Telescope
  {
    "nvim-telescope/telescope.nvim",
    cmd = "Telescope",
    keys = {
      { "<leader>f", "<CMD>Telescope find_files<CR>", desc = "Find files" },
      { "<leader>b", "<CMD>Telescope buffers<CR>", desc = "Switch buffer" },
      { "<leader>j", "<CMD>Telescope jumplist<CR>", desc = "Jumplist" },
      { "<leader>?", "<CMD>Telescope builtin<CR>", desc = "Telescope builtin commands" },
    },
    config = function()
      require("plugins.config.telescope")
    end,
    dependencies = { "nvim-lua/plenary.nvim" },
  },

  -- Treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    event = "VeryLazy",
    build = ":TSUpdate",
    config = function()
      require("plugins.config.treesitter")
    end,
    dependencies = { "nvim-treesitter/nvim-treesitter-textobjects" },
  },

  -- Latex
  {
    "lervag/vimtex",
    ft = { "tex", "bib" },
    config = function()
      require("plugins.config.vimtex")
    end,
  },

  -- LSP
  {
    "neovim/nvim-lspconfig",
    event = "VeryLazy",
    config = function()
      require("plugins.config.lsp")
    end,
    dependencies = {
      "j-hui/fidget.nvim",
      "onsails/lspkind-nvim",
    },
  },

  -- Completion
  {
    "hrsh7th/nvim-cmp",
    event = "InsertEnter",
    config = function()
      require("plugins.config.completion")
    end,
    dependencies = {
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-nvim-lua",
    },
  },

  -- Snippets
  {
    "L3MON4D3/LuaSnip",
    event = "InsertEnter",
    config = function()
      require("plugins.config.luasnip")
    end,
    dependencies = { "saadparwaiz1/cmp_luasnip" },
  },
}
