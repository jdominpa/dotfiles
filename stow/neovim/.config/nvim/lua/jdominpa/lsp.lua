local lsp = {}

local nnoremap = function (lhs, rhs)
  vim.api.nvim_buf_set_keymap(0, 'n', lhs, rhs, {noremap = true, silent = true})
end

local on_attach = function ()
  print("LSP started.")

  local mappings = {
    ['<Leader>ld'] = '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>',
    ['<Leader>ca'] = '<cmd>lua vim.lsp.buf.code_action()<CR>',
    ['<Leader>cr'] = '<cmd>lua vim.lsp.buf.rename()<CR>',
    ['<Leader>cf'] = '<cmd>lua vim.lsp.buf.formatting()<CR>',
    ['<c-]>'] = '<cmd>lua vim.lsp.buf.definition()<CR>',
    ['K'] = '<cmd>lua vim.lsp.buf.hover()<CR>',
    ['gd'] = '<cmd>lua vim.lsp.buf.declaration()<CR>',
    ['gD'] = '<cmd>lua vim.lsp.buf.implementation()<CR>',
    ['1gD'] = '<cmd>lua vim.lsp.buf.type_definition()<CR>',
    ['gr'] = '<cmd>lua vim.lsp.buf.references()<CR>',
  }

  for lhs, rhs in pairs(mappings) do
    nnoremap(lhs, rhs)
  end

  vim.api.nvim_win_set_option(0, 'signcolumn', 'yes')

  require('completion').on_attach()
end

lsp.bind = function ()
  pcall(function ()
    if vim.api.nvim_win_get_var(0, 'textDocument/hover') then
      nnoremap('K', ':call nvim_win_close(0, v:true)<CR>')
      nnoremap('<Esc>', ':call nvim_win_close(0, v:true)<CR>')

      vim.api.nvim_win_set_option(0, 'cursorline', false)

      -- I believe this is supposed to happen automatically because I can see
      -- this in lsp/util.lua:
      --
      --     api.nvim_buf_set_option(floating_bufnr, 'modifiable', false)
      --
      -- but it doesn't seem to be working.
      vim.api.nvim_buf_set_option(0, 'modifiable', false)
    end
  end)
end

lsp.init = function ()
  require('lspconfig').vimls.setup{
    on_attach = on_attach,
  }

  require('lspconfig').clangd.setup{
    on_attach = on_attach,
  }

  require('lspconfig').hls.setup{
    on_attach = on_attach,
    root_dir = dirname,
  }

  require('lspconfig').pyright.setup{}

  require('lspconfig').texlab.setup{
    on_attach = on_attach,
    latex = {
      build = {
        args = { "-pdf", "-interaction=nonstopmode", "-synctex=1" },
        executable = "latexmk",
        onSave = false
      },
      forwardSearch = {
        args = {},
        onSave = false
      },
      lint = {
        onChange = false
      },
    },
  }

  -- Override hover winhighlight.
  local method = 'textDocument/hover'
  local hover = vim.lsp.handlers[method]
  vim.lsp.handlers[method] = function (_, method, result)
     hover(_, method, result)

     for _, winnr in ipairs(vim.api.nvim_tabpage_list_wins(0)) do
       if pcall(function ()
         vim.api.nvim_win_get_var(winnr, 'textDocument/hover')
       end) then
         vim.api.nvim_win_set_option(winnr, 'winhighlight', 'Normal:Visual,NormalNC:Visual')
         break
       else
         -- Not a hover window.
       end
     end
  end
end

return lsp
