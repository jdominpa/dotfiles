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
local n = require("luasnip.extras").noempty

-- Expand conditions
local line_begin = require("luasnip.extras.expand_conditions").line_begin
local in_mathzone = function()
  return vim.fn['vimtex#syntax#in_mathzone']() == 1
end

-- Snippets
local snippets, autosnippets = {}, {}

-- local package = s(
--   {
--     trig = "pkg",
--     dscr = "Add packge with optional parameters",
--   },
--   -- FIXME: for some reason this doesn't work
--   -- fmta("\usepackage{}", {
--   --   -- c(1, { sn(nil, { t("["), i(1), t("]") }), t("") }),
--   --   -- i(2),
--   -- }),
--   { condition = line_begin }
-- )
-- table.insert(snippets, package)

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

local inline_math = s(
  {
    trig = "([^%a])mm",
    dscr = "Inline math",
    regTrig = true,
    hidden = true,
  },
  fmta("<>$<>$", {
    f(function(_, snip) return snip.captures[1] end),
    i(1),
  })
)
table.insert(autosnippets, inline_math)

local display_math = s(
  {
    trig = "dm",
    dscr = "Display math",
    hidden = true,
  },
  fmta([[
  \[
      <>
  \]
  ]],
  i(1),
  { condition = line_begin })
)
table.insert(autosnippets, display_math)

local eq_env = s(
  {
    trig = "eq",
    dscr = "Equation environment",
  },
  fmta([[
  \begin{equation}
      <>
  \end{equation}
  ]],
  i(1)),
  { condition = line_begin }
)
table.insert(snippets, eq_env)

-- TODO: the snippet doesn't expand
local fraction = s(
  {
    trig = "ff",
    dscr = "Fraction",
    hidden = true,
  },
  fmta("\frac{<>}{<>}", {
    i(1),
    i(2),
  }),
  { condition = in_mathzone }
)
table.insert(autosnippets, fraction)

return snippets, autosnippets
