local keymap = require("jdominpa.keymap")
local nnoremap = keymap.nnoremap

-- LSP keymappings
local on_attach = function(client, bufnr)
  local opts = { buffer = bufnr }
  nnoremap("gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
  nnoremap("K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
  nnoremap("<leader>k", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
  nnoremap("<leader>r", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
  nnoremap("<leader>a", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
  nnoremap("[d", "<cmd>lua vim.diagnostic.goto_prev()<CR>", opts)
  nnoremap("]d", "<cmd>lua vim.diagnostic.goto_next()<CR>", opts)

  local has_telescope, telescope = pcall(require, "telescope.builtin")
  if has_telescope then
    nnoremap("gd", telescope.lsp_definitions, opts)
    nnoremap("gy", telescope.lsp_type_definitions, opts)
    nnoremap("gi", telescope.lsp_implementations, opts)
    nnoremap("gr", telescope.lsp_references, opts)
    nnoremap("<leader>s", telescope.lsp_document_symbols, opts)
    nnoremap("<leader>S", telescope.lsp_workspace_symbols, opts)
    nnoremap("<leader>d", telescope.diagnostics, opts)
    nnoremap("<leader>D", telescope.diagnostics, { buffer = nil })
  end
end

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

local servers = { "clangd", "texlab" }
for _, lsp in pairs(servers) do
  require("lspconfig")[lsp].setup({
    on_attach = on_attach,
    capabilities = capabilities,
  })
end

-- Lua lsp
local user = vim.fn.expand("$USER")
local lua_ls_root_path = "/home/" .. user .. "/.local/bin/lua-language-server"
local lua_ls_binary = lua_ls_root_path .. "/bin/lua-language-server"
require("lspconfig").lua_ls.setup({
  cmd = { lua_ls_binary, "-E", lua_ls_root_path .. "/main.lua" },
  on_attach = on_attach,
  capabilities = capabilities,
  settings = {
    Lua = {
      diagnostics = {
        -- Get the language server to recognize the `vim` global variable
        globals = { "vim" },
      },
    },
  },
})

-- LSP status information
require("fidget").setup()
