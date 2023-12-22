return {
  {
    "williamboman/mason.nvim",
    cmd = "Mason",
    build = ":MasonUpdate",
    config = function()
      require("mason").setup({
        ui = { border = "rounded" },
      })
    end
  },

  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
    },
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
          map("i", "<C-k>", vim.lsp.buf.signature_help, opts)
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

      local lspconfig = require("lspconfig")

      -- Rounded window borders
      require("lspconfig.ui.windows").default_options.border = "rounded"

      -- Install lsp servers
      require("mason-lspconfig").setup({
        ensure_installed = {
          "lua_ls",
          "clangd",
          "pyright",
          "texlab",
        },
      })

      -- Setup lsp servers
      local servers = { "clangd", "pyright", "texlab" }
      for _, server in pairs(servers) do
        lspconfig[server].setup({
          capabilities = capabilities,
        })
      end

      lspconfig.lua_ls.setup({
        capabilities = capabilities,
        settings = {
          Lua = {
            workspace = {
              checkThirdParty = false,
              library = { vim.env.VIMRUNTIME },
            },
          },
        },
      })
    end,
  },
}
