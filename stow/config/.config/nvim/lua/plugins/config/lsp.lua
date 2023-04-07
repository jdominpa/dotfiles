local map = vim.keymap.set

-- LSP keymappings
local on_attach = function(client, bufnr)
  local opts = { buffer = bufnr }
  map("n", "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
  map("n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
  map("n", "<c-s>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
  map("n", "<leader>lr", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
  map("n", "<leader>la", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
  map("n", "[d", "<cmd>lua vim.diagnostic.goto_prev()<CR>", opts)
  map("n", "]d", "<cmd>lua vim.diagnostic.goto_next()<CR>", opts)

  local has_telescope, telescope = pcall(require, "telescope.builtin")
  if has_telescope then
    map("n", "gd", telescope.lsp_definitions, opts)
    map("n", "gy", telescope.lsp_type_definitions, opts)
    map("n", "gi", telescope.lsp_implementations, opts)
    map("n", "gr", telescope.lsp_references, opts)
    map("n", "<leader>ls", telescope.lsp_document_symbols, opts)
    map("n", "<leader>lS", telescope.lsp_workspace_symbols, opts)
    map("n", "<leader>ld", telescope.diagnostics, opts)
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
