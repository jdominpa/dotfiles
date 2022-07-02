require("nvim-treesitter.configs").setup {
  ensure_installed = { "lua", "c", "cpp", "python", "latex" },
  highlight = {
    enable = true,
    disable = {},
  },
  indent = {
    enable = false,
    disable = {},
  },
}
