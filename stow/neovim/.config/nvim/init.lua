pcall(require, "require not loaded")

if require "jdominpa.first_load"() then
  return
end

-- Leader key -> ","
vim.g.mapleader = ","

-- Snippets plugin
vim.g.snippets = "luasnip"

-- Plugins
require "jdominpa.packer"

-- Telescope
require "jdominpa.telescope.setup"
require "jdominpa.telescope.mappings"
