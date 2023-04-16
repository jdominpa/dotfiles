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
local make_condition = require("luasnip.extras.conditions").make_condition

local in_mathzone = make_condition(function()
  return vim.fn['vimtex#syntax#in_mathzone']() == 1
end)

-- Snippets
local snippets, autosnippets = {}, {}

local geq = s(
  {
    trig = "geq",
    dscr = "Greater than or equal",
    hidden = true,
  },
  t("\\geq"),
  { condition = in_mathzone }
)
table.insert(autosnippets, geq)

local leq = s(
  {
    trig = "leq",
    dscr = "Less than or equal",
    hidden = true,
  },
  t("\\leq"),
  { condition = in_mathzone }
)
table.insert(autosnippets, leq)

local forall = s(
  {
    trig = "AA",
    dscr = "For all",
    hidden = true,
  },
  t("\\forall"),
  { condition = in_mathzone }
)
table.insert(autosnippets, forall)

local exists = s(
  {
    trig = "EE",
    dscr = "Exists",
    hidden = true,
  },
  t("\\exists"),
  { condition = in_mathzone }
)
table.insert(autosnippets, exists)

local ldots = s(
  {
    trig = "...",
    dscr = "Math mode ellipsis",
    wordTrig = false,
    hidden = true,
  },
  t("\\dotsc"),
  { condition = in_mathzone }
)
table.insert(autosnippets, ldots)

return snippets, autosnippets
