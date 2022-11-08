local keymap = require("jdominpa.keymap")
local nnoremap = keymap.nnoremap
local inoremap = keymap.inoremap
local vnoremap = keymap.vnoremap
local xnoremap = keymap.xnoremap

-- Make Y behave like the rest of capital actions
nnoremap("Y", "yg$")

-- Center search results when jumping between them
nnoremap("n", "nzzzv")
nnoremap("N", "Nzzzv")

-- Don't move the screen when joining lines
nnoremap("J", "mzJ`z")

-- Move lines
inoremap("A-j", "<Esc>:m .+1<CR>==gi")
inoremap("A-k", "<Esc>:m .-2<CR>==gi")
vnoremap("A-j", ":m '>+1<CR>gv=gv")
vnoremap("A-k", ":m '<-2<CR>gv=gv")

-- Buffer keymaps
nnoremap("<leader>bn", ":bn<CR>")
nnoremap("<leader>bp", ":bp<CR>")
nnoremap("<leader>bd", ":bd<CR>")
