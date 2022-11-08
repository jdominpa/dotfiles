local keymap = require("jdominpa.keymap")
local nnoremap = keymap.nnoremap

-- LSP keymappings
local on_attach = function(client, bufnr)
  local opts = { buffer = bufnr }

  nnoremap("gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
  nnoremap("gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
  nnoremap("gT", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
  nnoremap("gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
  nnoremap("gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
  nnoremap("K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
  nnoremap("<C-s>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
  nnoremap("<localleader>lr", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
  nnoremap("<localleader>la", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
  nnoremap("[d", "<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>", opts)
  nnoremap("]d", "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>", opts)
end

local capabilities = require('cmp_nvim_lsp').default_capabilities(vim.lsp.protocol.make_client_capabilities())

local servers = { "sumneko_lua", "clangd", "pyright", "ltex" }
for _, lsp in pairs(servers) do
  require('lspconfig')[lsp].setup {
    on_attach = on_attach,
    capabilities = capabilities,
  }
end
