local ls = require("luasnip") 
local s = ls.s
local i = ls.i
local t = ls.t
local c = ls.choice_node
local f = ls.function_node
local d = ls.dynamic_node
local sn = ls.snippet_node

local fmt = require("luasnip.extras.fmt").fmt
local rep = require("luasnip.extras").rep

local snippets, autosnippets = {}, {}

local for_loop = s(
  {
    trig = "for([%w_]+)",
    regTrig = true,
    hidden = true,
  },
  fmt([[
  for (int {} = {}; {} < {}; {}++) {{
      {}
  }}
  ]],
  {
    d(1, function(_, snip)
      return sn(1, i(1, snip.captures[1]))
    end),
    i(2, "0"),
    rep(1),
    i(3, "n"),
    rep(1),
    i(4, "/* TODO */"),
  }))
table.insert(snippets, for_loop)

return snippets, autosnippets
