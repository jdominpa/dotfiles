if vim.g.snippets ~= "luasnip" or not pcall(require, "luasnip") then
  return
end

require("luasnip.loaders.from_lua").load({ paths = "$HOME/.config/nvim/snippets/" })

local ls = require("luasnip")
ls.config.set_config {
  history = true,
  updateevents = "TextChanged,TextChangedI",
  enable_autosnippets = true,
  ext_opts = {
    [require("luasnip.util.types").choiceNode] = {
      active = {
        virt_text = { { " « ", "GruvboxOrange" } },
      },
    },
  },
}

local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local c = ls.choice_node
local f = ls.function_node
local d = ls.dynamic_node
local sn = ls.snippet_node
local rep = require("luasnip.extras").rep
local fmt = require("luasnip.extras.fmt").fmt

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

vim.keymap.set("n", "<leader>s", "<cmd>LuaSnipEdit<CR>", { silent = true, noremap = true })
