if not pcall(require, "nvim-treesitter") then
  return
end

require("nvim-treesitter.configs").setup {
  ensure_installed = "all",
  sync_install = false,
  ignore_install = { "latex" },
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
  },
}
