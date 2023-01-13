pcall(require, "require not loaded")

if require "jdominpa.first_load"() then
  return
end

-- Leader key -> <Space>
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- Snippets plugin
vim.g.snippets = "luasnip"

-- Options
require "jdominpa.options"

-- Plugins
require "jdominpa.packer"

-- Telescope
require "jdominpa.telescope"
