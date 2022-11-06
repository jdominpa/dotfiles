local testSnippet = s("test", {
  t("This is a test snippet"),
  i(1, " placeholder"),
})
table.insert(snippets, testSnippet)

return snippets
