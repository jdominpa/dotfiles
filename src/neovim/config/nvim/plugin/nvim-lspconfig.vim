lua require('jdominpa.lsp').init()

sign define LspDiagnosticsErrorSign text=✖
sign define LspDiagnosticsWarningSign text=⚠
sign define LspDiagnosticsInformationSign text=ℹ
sign define LspDiagnosticsHintSign text=➤

augroup JdominpaLanguageClientAutocmds
  autocmd!
  autocmd ColorScheme * lua require('jdominpa.lsp').set_up_highlights()
  autocmd WinEnter * lua require('jdominpa.lsp').bind()
augroup END
