if not pcall(require, "lspconfig") then
  return
end

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
  nnoremap("[d", "<cmd>lua vim.diagnostic.goto_prev()<CR>", opts)
  nnoremap("]d", "<cmd>lua vim.diagnostic.goto_next()<CR>", opts)
end

local servers = { "clangd" }
for _, lsp in pairs(servers) do
  require("lspconfig")[lsp].setup({
    on_attach = on_attach,
  })
end

-- Lua lsp
local user = vim.fn.expand("$USER")
local sumneko_root_path = "/home/" .. user .. "/.local/bin/lua-language-server"
local sumneko_binary = sumneko_root_path .. "/bin/lua-language-server"
require("lspconfig").sumneko_lua.setup({
  cmd = { sumneko_binary, "-E", sumneko_root_path .. "/main.lua" },
  on_attach = on_attach,
  settings = {
    Lua = {
      diagnostics = {
        -- Get the language server to recognize the `vim` global variable
        globals = { "vim" },
      },
      workspace = {
        -- Make the server aware of neovim runtime files
        library = {
          [vim.fn.expand("$VIMRUNTIME/lua")] = true,
          [vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true,
        },
      },
    },
  },
})
