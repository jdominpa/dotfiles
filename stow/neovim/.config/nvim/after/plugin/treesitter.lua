require("nvim-treesitter.configs").setup {
  ensure_installed = { "c", "cpp", "lua", "python" },
  sync_install = false,

  highlight = {
    enable = true,
  },
}
