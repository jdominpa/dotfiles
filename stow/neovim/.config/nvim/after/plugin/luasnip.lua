local ls = require "luasnip"
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_node

ls.config.set_config {
  history = true,
  enable_autosnippets = true,
}

ls.snippets = {
  all = {
    s("fn", {
      t("//Parameters: "),
      f(copy, 2),
      t({ "", "function " }),
      i(1),
      t("("),
      i(2, "int foo"),
      t({ ") {", "\t" }),
      i(0),
      t({ "", "}" }),
    }),
  },
  c = {
    s("for", {
      t("for ("), i(1, "i"), t(" = "), i(2, "0"), t("; "), f(copy, 1), t(" < "), i(3, "n"), t("; "), f(copy, 1), t({ "++) {", "\t" }),
      i(0),
      t({ "", "}" }),
    }),
    s("if", {
      t("if ("), i(1, "condition"), t({ ") {", "\t" }),
      i(0),
      t({ "", "}" }),
    }),
  },
}
