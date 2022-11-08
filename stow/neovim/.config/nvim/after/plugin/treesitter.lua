require("nvim-treesitter.configs").setup {
  ensure_installed = { "latex", "c", "cpp", "lua", "python" },
  sync_install = false,

  highlight = {
    enable = true,
  },
}
