if not pcall(require, "telescope") then
  return
end

local keymap = require("jdominpa.keymap")
local nnoremap = keymap.nnoremap

nnoremap("<leader>f", require("telescope.builtin").find_files)
nnoremap("<leader>b", require("telescope.builtin").buffers)
nnoremap("<leader>j", require("telescope.builtin").jumplist)
nnoremap("<leader>?", require("telescope.builtin").builtin)

nnoremap("<leader>gs", require("telescope.builtin").git_status)
nnoremap("<leader>gc", require("telescope.builtin").git_commits)
