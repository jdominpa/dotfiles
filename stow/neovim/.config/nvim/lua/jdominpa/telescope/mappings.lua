TelescopeMapArgs = TelescopeMapArgs or {}

local map_tele = function(key, f, options, buffer)
  local map_key = vim.api.nvim_replace_termcodes(key .. f, true, true, true)

  TelescopeMapArgs[map_key] = options or {}

  local mode = "n"
  local rhs = string.format("<cmd>lua require('telescope.builtin').%s(TelescopeMapArgs['%s'])<CR>", f, map_key)

  if not buffer then
    local map_options = { noremap = true, silent = true }
  else
    local map_options = { noremap = true, silent = true, buffer = 0 }
  end

  vim.keymap.set(mode, key, rhs, map_options)
end

map_tele("<space>xt", "builtin")
map_tele("<space>xf", "find_files")
map_tele("<space>xr", "oldfiles")
map_tele("<space>xb", "buffers")
-- map_tele("<space>ft", "treesitter")

map_tele("<space>ho", "vim_options")
map_tele("<space>hk", "keymaps")

map_tele("<space>cgs", "git_status")
map_tele("<space>cgc", "git_commits")
