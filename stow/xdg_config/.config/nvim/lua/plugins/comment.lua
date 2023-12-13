return {
  {
    "numToStr/Comment.nvim",
    event = "VeryLazy",
    config = function()
      require("Comment").setup({
        opleader = {
          line = "gc",
          block = "gb",
        },
        mappings = {
          basic = true,
          extra = true,
        },
        toggler = {
          line = "gcc",
          block = "gbc",
        },
        pre_hook = nil,
        post_hook = nil,
        ignore = nil,
      })
    end,
  },
}
