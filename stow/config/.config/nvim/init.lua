pcall(require, "require not loaded")

-- Leader key -> <Space>
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- Options
require "jdominpa.options"

-- Plugins
require "jdominpa.lazy"

-- Telescope
require "jdominpa.telescope"
