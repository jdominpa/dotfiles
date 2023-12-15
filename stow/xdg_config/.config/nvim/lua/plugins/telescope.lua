return {
  {
    "nvim-telescope/telescope.nvim",
    cmd = "Telescope",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope-ui-select.nvim",
    },
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
      local telescope = require("telescope")
      telescope.setup({
        defaults = {
          prompt_prefix = "> ",
          selection_caret = "> ",
          winblend = 0,
          layout_strategy = "horizontal",
          layout_config = {
            width = 0.9,
            height = 0.85,
          },
          scroll_strategy = "cycle",
          color_devicons = true,
        },
        extensions = {
          ["ui-select"] = {
            require("telescope.themes").get_dropdown()
          },
        }
      })

      telescope.load_extension("ui-select")
    end,
  },
}
