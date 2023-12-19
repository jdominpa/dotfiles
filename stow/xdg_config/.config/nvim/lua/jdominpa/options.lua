local opt = vim.opt

-- Netrw options
vim.g.netrw_browse_split = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25

-- Ignore compiled files
opt.wildignore = "__pycache__"
opt.wildignore = opt.wildignore + { "*.o", "*~", "*.pyc", "pycache" }

opt.pumblend = 17
opt.wildmode = "longest:full"
opt.wildoptions = "pum"

opt.shortmess = opt.shortmess + "I"
opt.showmode = false
opt.laststatus = 3          -- Global status line
opt.showmatch = true        -- Show matching brackets when text indicator is over them
opt.relativenumber = true   -- Line numbers
opt.number = true           -- Show the line we are on
opt.ignorecase = true       -- Ignore case when searching ...
opt.smartcase = true        -- ... unless there's a capital letter in the query
opt.equalalways = false     -- Don't change window size everytime we open or close one
opt.splitright = true       -- Split windows to the right
opt.splitbelow = true       -- Split windows below
opt.hlsearch = false        -- Highlight searched term
opt.incsearch = true        -- Highlight search matchings as we type

-- Tabs
opt.smartindent = true
opt.tabstop = 4
opt.shiftwidth = 4
opt.softtabstop = 4
opt.expandtab = true

opt.breakindent = true              -- Visually indent wrapped lines
opt.showbreak = string.rep("↳ ", 3) -- Wrap long lines smartly
opt.linebreak = true
opt.list = true                     -- Show tabs and trailing whitespace

opt.inccommand = "split"            -- Preview :s command results
opt.swapfile = false                -- Don't create swapfiles

opt.formatoptions = opt.formatoptions
  - "a" -- Don't auto format
  - "t" -- Don't auto format code
  + "c" -- Auto-wrap comments using textwidth
  + "q" -- Allow formatting of comments with "gq"
  + "r" -- Enter continues comments
  - "o" -- O and o don't continue comments
  + "n" -- Format lists correctly
  + "j" -- Auto-remove comments if possible
  - "2" -- Don't indent the first line of a paragraph
