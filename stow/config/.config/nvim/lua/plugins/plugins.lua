return {
  -- Colorscheme
  {
    "catppuccin/nvim",
    name = "catppuccin",
    config = function()
      require("plugins.config.catppuccin")
    end,
  },

  -- Status line
  {
    "nvim-lualine/lualine.nvim",
    event = "VeryLazy",
    config = function()
      require("plugins.config.lualine")
    end,
    dependencies = { "kyazdani42/nvim-web-devicons", lazy = true },
  },

  -- Utilities
  {
    "kylechui/nvim-surround",
    event = "VeryLazy",
    config = function()
      require("nvim-surround").setup()
    end,
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
      { "<leader>s", "<CMD>Telescope live_grep<CR>", desc = "Live grep in cwd" },
      { "<leader>?", "<CMD>Telescope builtin<CR>", desc = "Telescope builtin commands" },
      { "<leader>gs", "<CMD>Telescope git_status<CR>", desc = "Git status" },
      { "<leader>gc", "<CMD>Telescope git_commits<CR>", desc = "Git commits" },
    },
    config = function()
      require("plugins.config.telescope")
    end,
    dependencies = { "nvim-lua/plenary.nvim" },
  },

  -- Treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    event = { "BufReadPost", "BufNewFile" },
    config = function()
      require("plugins.config.treesitter")
    end,
    dependencies = { "nvim-treesitter/nvim-treesitter-textobjects" },
  },

  -- LSP
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      require("plugins.config.lsp")
    end,
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
      "onsails/lspkind-nvim",
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

  -- Latex
  {
    "lervag/vimtex",
    ft = { "tex", "bib" },
    config = function()
      require("plugins.config.vimtex")
    end,
  },
}
