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

map_tele("<leader>.", "find_files")
map_tele("<leader>r", "oldfiles")
map_tele("<leader>b", "buffers")

map_tele("<leader>ho", "vim_options")
map_tele("<leader>hk", "keymaps")

map_tele("<leader>gs", "git_status")
map_tele("<leader>gc", "git_commits")
