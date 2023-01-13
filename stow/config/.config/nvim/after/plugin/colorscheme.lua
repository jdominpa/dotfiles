vim.opt.termguicolors = true
vim.opt.background = "dark"
if not pcall(vim.cmd, "colorscheme catppuccin_mocha") then
  return
end
