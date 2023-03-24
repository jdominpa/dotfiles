local keymap = require("jdominpa.keymap")
local nmap = keymap.nmap
local nnoremap = keymap.nnoremap
local inoremap = keymap.inoremap
local vnoremap = keymap.vnoremap
local xnoremap = keymap.xnoremap

-- Open netrw
nnoremap("<leader>F", ":Ex<CR>")

-- Make Y behave like the rest of capital actions
nnoremap("Y", "yg$")

-- Some helix imported mappings
nnoremap("gh", "0")
vnoremap("gh", "0")
nnoremap("gl", "$")
vnoremap("gl", "$")
nnoremap("gs", "^")
vnoremap("gs", "^")
nnoremap("gn", ":bn<CR>")
nnoremap("gp", ":bp<CR>")

-- Center search results when jumping between them
nnoremap("n", "nzzzv")
nnoremap("N", "Nzzzv")

-- Center cursor when scrolling up or down
nnoremap("<C-d>", "<C-d>zz")
nnoremap("<C-u>", "<C-u>zz")

-- Don't move the screen when joining lines
nnoremap("J", "mzJ`z")

-- Move lines
vnoremap("J", ":m '>+1<CR>gv=gv")
vnoremap("K", ":m '<-2<CR>gv=gv")

-- Replace highlighted text without losing yanked text
xnoremap("<leader>p", "\"_dP")

-- Yank to "+ register
nnoremap("<leader>y", "\"+y")
vnoremap("<leader>y", "\"+y")
nmap("<leader>Y", "\"+Y")

-- Delete without yanking text
nnoremap("<leader>d", "\"_d")
vnoremap("<leader>d", "\"_d")
nnoremap("<leader>D", "\"_D")

-- Jump to next or previous error in quickfix list
nnoremap("<C-j>", "<cmd>cnext<CR>zz")
nnoremap("<C-k>", "<cmd>cprev<CR>zz")
nnoremap("<leader>j", "<cmd>lnext<CR>zz")
nnoremap("<leader>k", "<cmd>lprev<CR>zz")
