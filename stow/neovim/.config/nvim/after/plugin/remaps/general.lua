local keymap = require("jdominpa.keymap")
local nmap = keymap.nmap
local nnoremap = keymap.nnoremap
local inoremap = keymap.inoremap
local vnoremap = keymap.vnoremap
local xnoremap = keymap.xnoremap

-- Open netrw
nnoremap("<leader>xd", ":Ex<CR>")

-- Make Y behave like the rest of capital actions
nnoremap("Y", "yg$")

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

-- TODO: Jump to next or previous error in quickfix list
nnoremap("<C-k>", "<cmd>cnext<CR>zz")
nnoremap("<C-j>", "<cmd>cprev<CR>zz")
nnoremap("<leader>k", "<cmd>lnext<CR>zz")
nnoremap("<leader>j", "<cmd>lprev<CR>zz")
