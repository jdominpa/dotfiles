return {
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      -- Keymappings
      local map = vim.keymap.set

      map("n", "[d", vim.diagnostic.goto_prev)
      map("n", "]d", vim.diagnostic.goto_next)

      vim.api.nvim_create_autocmd("LspAttach", {
        group = vim.api.nvim_create_augroup("UserLspConfig", {}),
        callback = function(ev)
          vim.bo[ev.buf].omnifunc = "v:lua.vim.lsp.omnifunc"

          local opts = { buffer = ev.buf }
          map("n", "gD", vim.lsp.buf.declaration, opts)
          map("n", "K", vim.lsp.buf.hover, opts)
          map("i", "<C-s>", vim.lsp.buf.signature_help, opts)
          map("n", "<leader>lr", vim.lsp.buf.rename, opts)
          map("n", "<leader>la", vim.lsp.buf.code_action, opts)
          map("n", "<leader>lf", function()
            vim.lsp.buf.format({ async = true })
          end, opts)

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
      })

      -- Inform of cmp extra capabilities
      local capabilities = vim.lsp.protocol.make_client_capabilities()
      capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

      -- General lsp configuration
      local lspconfig = require("lspconfig")

      local servers = { "clangd", "texlab" }
      for _, lsp in pairs(servers) do
        lspconfig[lsp].setup({
          capabilities = capabilities,
        })
      end

      -- Lua lsp
      local user = vim.fn.expand("$USER")
      local lua_ls_root_path = "/home/" .. user .. "/.local/bin/lua-language-server"
      local lua_ls_binary = lua_ls_root_path .. "/bin/lua-language-server"
      lspconfig.lua_ls.setup({
        cmd = { lua_ls_binary, "-E", lua_ls_root_path .. "/main.lua" },
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
    end,
  },
}
