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
local line_begin = require("luasnip.extras.expand_conditions").line_begin

local in_text = make_condition(function()
  return vim.fn['vimtex#syntax#in_mathzone']() == 0
end)

local in_mathzone = make_condition(function()
  return vim.fn['vimtex#syntax#in_mathzone']() == 1
end)

-- Snippets
local snippets, autosnippets = {}, {}

local inline_math = s(
  {
    trig = "mm",
    dscr = "Inline math environment",
    hidden = true,
  },
  fmta("\\(<>\\)", i(1)),
  { condition = in_text }
)
table.insert(autosnippets, inline_math)

local display_math = s(
  {
    trig = "dm",
    dscr = "Display math environment",
    hidden = true,
  },
  fmta([[
  \[
      <>
  \]
  ]], i(1)),
  { condition = line_begin * in_text }
)
table.insert(autosnippets, display_math)

local fraction = s(
  {
    trig = "ff",
    dscr = "Fraction",
    hidden = true,
  },
  fmta("\\frac{<>}{<>}", { i(1), i(2) }),
  { condition = in_mathzone }
)
table.insert(autosnippets, fraction)

local subindex = s(
  {
    trig = "sd",
    dscr = "Subindex",
    wordTrig = false,
    hidden = true,
  },
  fmta("_<>", c(1, { i(nil), sn(nil, fmta("{<>}", i(1))) })),
  { condition = in_mathzone }
)
table.insert(autosnippets, subindex)

local superindex = s(
  {
    trig = "sp",
    dscr = "Superindex",
    wordTrig = false,
    hidden = true,
  },
  fmta("^<>", c(1, { i(nil), sn(nil, fmta("{<>}", i(1))) })),
  { condition = in_mathzone }
)
table.insert(autosnippets, superindex)

local partial_derivative = s(
  {
    trig = "pd",
    dscr = "Partial derivative",
    hidden = true,
  },
  t("\\partial"),
  { condition = in_mathzone }
)
table.insert(autosnippets, partial_derivative)

return snippets, autosnippets
