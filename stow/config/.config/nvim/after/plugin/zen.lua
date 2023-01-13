if not pcall(require, "zen-mode") then
  return
end

require("zen-mode").setup {
  window = {
    backdrop = 0.90,
    options = {
      number = false,
      relativenumber = false,
    },
  },
}
