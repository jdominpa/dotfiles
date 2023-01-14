vim.opt.termguicolors = true
vim.opt.background = "dark"

local has_theme, theme = pcall(require, "catppuccin")
if not has_theme then
  return
end
theme.setup({
  flavour = "mocha",
  no_italic = true,
})
vim.cmd [[colorscheme catppuccin]]
