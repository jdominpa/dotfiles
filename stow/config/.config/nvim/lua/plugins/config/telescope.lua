local actions = require("telescope.actions")

require("telescope").setup {
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
    mappings = {
      i = {
        ["<C-h>"] = "which_key"
      },
      n = {
        ["q"] = actions.close
      },
    },
  },
}
