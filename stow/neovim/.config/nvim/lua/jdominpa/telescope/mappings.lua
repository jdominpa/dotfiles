local map_tele_builtin = function(mode, key, f)
  local opts = { noremap = true, silent = true }
  local rhs = string.format("<cmd>lua require('telescope.builtin').%s()<CR>", f)
  vim.api.nvim_set_keymap(mode, key, rhs, opts)
end

map_tele_builtin("n", "<space>ff", "find_files")
map_tele_builtin("n", "<space>fo", "oldfiles")
map_tele_builtin("n", "<space>fb", "buffers")
map_tele_builtin("n", "<space>ft", "treesitter")

map_tele_builtin("n", "<space>xo", "vim_options")
map_tele_builtin("n", "<space>xk", "keymaps")
map_tele_builtin("n", "<space>xb", "builtin")

map_tele_builtin("n", "<space>gs", "git_status")
map_tele_builtin("n", "<space>gc", "git_commits")
