if vim.g.snippets ~= "luasnip" or not pcall(require, "luasnip") then
  return
end

-- Snippet configuration
local ls = require("luasnip")
local s = ls.snippet
local sn = ls.snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_node
local events = require("luasnip.util.events")
local fmt = require("luasnip.extras.fmt").fmt

ls.config.set_config {
  history = true,
  updateevents = "TextChanged,TextChangedI",
  enable_autosnippets = true,
}

-- Keybindings
vim.keymap.set({ "i", "s" }, "<C-j>", function()
  if ls.expand_or_jumpable() then
    ls.expand_or_jump()
  end
end, { silent = true })

vim.keymap.set({ "i", "s" }, "<C-k>", function()
  if ls.jumpable(-1) then
    ls.jump(-1)
  end
end, { silent = true })

vim.keymap.set("i", "<C-l>", function()
  if ls.choice_active() then
    ls.change_choice(1)
  end
end)

vim.keymap.set("n", "<leader><leader>s", "<cmd>source $HOME/.config/nvim/after/plugin/luasnip.lua<CR>")

-- Friendly snippets
require("luasnip.loaders.from_vscode").lazy_load()
