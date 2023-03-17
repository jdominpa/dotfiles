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

opt.showmode = false
opt.showcmd = true
opt.laststatus = 3          -- Global status line
opt.cmdheight = 1           -- Height of the command bar
opt.incsearch = true        -- Highlight search matchings as we type
opt.showmatch = true        -- Show matching brackets when text indicator is over them
opt.relativenumber = true   -- Line numbers
opt.number = true           -- Show the line we are on
opt.ignorecase = true       -- Ignore case when searching ...
opt.smartcase = true        -- ... unless there's a capital letter in the query
opt.hidden = true           -- Buffers stay around
opt.equalalways = false     -- Don't change window size everytime we open or close one
opt.splitright = true       -- Split windows to the right
opt.splitbelow = true       -- Split windows below
opt.hlsearch = false        -- Highlight searched term

-- Tabs
opt.autoindent = true
opt.tabstop = 4
opt.shiftwidth = 4
opt.softtabstop = 4
opt.expandtab = true

opt.breakindent = true
opt.showbreak = string.rep("↳ ", 3) -- Wrap long lines smartly
opt.linebreak = true
opt.list = true                     -- show whitespace
opt.listchars = {
  nbsp = '⦸',                       -- CIRCLED REVERSE SOLIDUS (U+29B8, UTF-8: E2 A6 B8)
  extends = '»',                    -- RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK (U+00BB, UTF-8: C2 BB)
  precedes = '«',                   -- LEFT-POINTING DOUBLE ANGLE QUOTATION MARK (U+00AB, UTF-8: C2 AB)
  tab = '> ',
  trail = '•',                      -- BULLET (U+2022, UTF-8: E2 80 A2)
}

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