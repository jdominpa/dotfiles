local M = {}

function keymap(mode, outer_opts)
  outer_opts = outer_opts or { noremap = true, silent = true }
  return function(lhs, rhs, opts)
    opts = vim.tbl_extend("force", outer_opts, opts or {})
    vim.keymap.set(mode, lhs, rhs, opts)
  end
end

M.nmap = keymap("n", { noremap = false, silent = true })
M.nnoremap = keymap("n")
M.inoremap = keymap("i")
M.vnoremap = keymap("v")
M.xnoremap = keymap("x")

return M
