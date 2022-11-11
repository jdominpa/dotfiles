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

-- Snippets
local snippets, autosnippets = {}, {}

local package = s(
  {
    trig = "pkg",
    dscr = "Add package with optional parameters",
  },
  fmta("\\usepackage<>{<>}", {
    c(1, { sn(nil, fmta("[<>]", i(1))), t("") }),
    i(2),
  })
)
table.insert(snippets, package)

return snippets, autosnippets
