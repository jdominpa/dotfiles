local remap = require("jdominpa.keymaps")
local nnoremap = remap.nnoremap
local inoremap = remap.inoremap
local vnoremap = remap.vnoremap
local xnoremap = remap.xnoremap

-- Make Y behave like the rest of capital actions
nnoremap("Y", "yg$")

-- Center search results when jumping between them
nnoremap("n", "nzzzv")
nnoremap("N", "Nzzzv")

-- Don't move the screen when joining lines
nnoremap("J", "mzJ`z")

-- Move lines
inoremap("M-j", "<Esc>:m .+1<CR>==gi")
inoremap("M-k", "<Esc>:m .-2<CR>==gi")
vnoremap("M-j", ":m '>+1<CR>gv=gv")
vnoremap("M-k", ":m '<-2<CR>gv=gv")
