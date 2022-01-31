require("nvim-treesitter.configs").setup {
  ensure_installed = { "lua", "c", "rust", "cpp", "python" },
  highlight = {
    enable = true,
    disable = {},
  },
  indent = {
    enable = false,
    disable = {},
  },
}
