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

-- Helper functions
_G.if_char_insert_space = function()
  if string.find(vim.v.char, "%a") then
    vim.v.char = " " .. vim.v.char
  end
end

local tex = {}

tex.in_mathzone = function()
  return vim.fn['vimtex#syntax#in_mathzone']() == 1
end

tex.in_text = function()
  return not tex.in_mathzone
end

tex.rec_ls = function()
  return sn(nil, {
    c(1, {
      t({""}),
      sn(nil, {t({"", "\t\\item "}), i(1), d(2, tex.rec_ls, {})}),
    }),
  });
end

ls.add_snippets("tex", {

  s("dm", {
    t({ "\\[", "\t" }),
    i(1),
    t({ "", "\\]" }),
  }, { condition = tex.in_text }),

  s("mk", { t("$"), i(1), t("$") }, {
    callbacks = {
      [-1] = {
        [events.leave] = function()
          vim.cmd([[
            autocmd InsertCharPre <buffer> ++once lua _G.if_char_insert_space()
          ]])
        end
      }
    }
  }),

  s("ls", {
    t({ "\\begin{itemize}", "\t\\item " }),
    i(1),
    d(2, tex.rec_ls, {}),
    t({ "", "\\end{itemize}" }),
  }),

})

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
