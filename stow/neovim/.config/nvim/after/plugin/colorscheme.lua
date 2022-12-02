vim.opt.termguicolors = true
vim.opt.background = "dark"
if not pcall(vim.cmd, "colorscheme gruvbox") then
  return
end
