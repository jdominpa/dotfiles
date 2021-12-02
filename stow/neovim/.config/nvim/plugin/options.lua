vim.g.loaded_matchparen = 1

local opt = vim.opt

-- Ignore compiled files
opt.wildignore = "__pycache__"
opt.wildignore = opt.wildignore + { "*.o", "*~", "*.pyc", "pycache" }

opt.pumblend = 17
opt.wildmode = "longest:full"
opt.wildoptions = "pum"

opt.showmode = false
opt.showcmd = true
opt.cmdheight = 1          -- Height of the command bar
opt.incsearch = true       -- Highlight search matchings as we type
opt.showmatch = true       -- Show matching brackets when text indicator is over them
opt.relativenumber = true  -- Line numbers
opt.number = true          -- Show the line we are on
opt.ignorecase = true      -- Ignore case when searching ...
opt.smartcase = true       -- ... unless there's a capital letter in the query
opt.hidden = true          -- Buffers stay around
opt.equalalways = false    -- Don't change window size everytime we open or close one
opt.splitright = true      -- Split windows to the right
opt.splitbelow = true      -- Split windows below
opt.hlsearch = true        -- Highlight searched term

-- Tabs
opt.autoindent = true
opt.tabstop = 4
opt.shiftwidth = 4
opt.softtabstop = 4
opt.expandtab = true

opt.wrap = true
opt.breakindent = true
opt.showbreak = string.rep(" ", 3)  -- Wrap long lines smartly
opt.linebreak = true

opt.foldmethod = "marker"
opt.foldlevel = 0
opt.modelines = 1

opt.belloff = "all"

opt.clipboard = "unnamedplus"

opt.inccommand = "split"
opt.swapfile = false

opt.mouse = "ni"

-- TODO: this option doesn't save correctly
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

opt.joinspaces = false

opt.fillchars = { eob = "~" }
