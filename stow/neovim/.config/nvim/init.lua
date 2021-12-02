pcall(require, "require not loaded")

if require "jdominpa.first_load"() then
  return
end

-- Leader key -> ","
vim.g.mapleader = ","

-- Plugins
require "jdominpa.plugins"

-- Neovim builtin LSP configuration
require "jdominpa.lsp"

-- Telescope
require "jdominpa.telescope.setup"
require "jdominpa.telescope.mappings"
