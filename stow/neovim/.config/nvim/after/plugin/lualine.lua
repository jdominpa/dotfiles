require("lualine").setup {
  options = {
    icons_enabled = true,
    theme = "modus-vivendi",
  },
  tabline = {
    lualine_a = {"buffers"},
    lualine_b = {},
    lualine_c = {},
    lualine_x = {},
    lualine_y = {},
    lualine_z = {"tabs"},
  },
}
