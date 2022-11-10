local ls = require("luasnip")
local s = ls.s
local i = ls.i
local t = ls.t
local c = ls.choice_node
local f = ls.function_node
local d = ls.dynamic_node
local sn = ls.snippet_node

local fmta = require("luasnip.extras.fmt").fmta
local rep = require("luasnip.extras").rep

-- Expand conditions
local line_begin = require("luasnip.extras.expand_conditions").line_begin
local in_text = function()
  return vim.fn['vimtex#syntax#in_mathzone']() == 0
end

-- Snippets
local snippets, autosnippets = {}, {}

local env = s(
  {
    trig = "env",
    dscr = "New environment",
  },
  fmta([[
  \begin{<>}
      <>
  \end{<>}
  ]], {
    i(1),
    i(2),
    rep(1),
  }),
  { condition = line_begin }
)
table.insert(snippets, env)

local enumerate = s(
  {
    trig = "enum",
    dscr = "Enumeration environment",
  },
  fmta([[
  \begin{enumerate}<>
      <>
  \end{enumerate}
  ]], {
    c(1, { t(""), t("[(i)]"), t("[(a)]") }),
    i(2),
  }),
  { condition = line_begin }
)
table.insert(snippets, enumerate)

return snippets, autosnippets
