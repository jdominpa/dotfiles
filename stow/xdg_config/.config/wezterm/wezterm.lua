local wezterm = require("wezterm")
local act = wezterm.action
local is_windows = wezterm.target_triple == "x86_64-pc-windows-msvc"
local config = wezterm.config_builder()

if is_windows then
  config.default_domain = "WSL:Ubuntu"
end

config.audible_bell = "Disabled"
config.color_scheme = "Catppuccin Mocha"
config.font_size = 13.0
config.font = wezterm.font("FiraCode Nerd Font Mono")
config.window_padding = {
  left = 0,
  right = 0,
  top = 0,
  bottom = 0,
}
config.hide_tab_bar_if_only_one_tab = true
config.window_close_confirmation = "NeverPrompt"

if not is_windows then
  config.term = "wezterm"
end

config.disable_default_key_bindings = true
config.keys = {
  { key = "n",      mods = "CTRL|SHIFT",    action = act.SpawnWindow },
  { key = "t",      mods = "CTRL|SHIFT",    action = act.SpawnTab("CurrentPaneDomain") },
  { key = "s",      mods = "CTRL|SHIFT",    action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
  { key = "v",      mods = "CTRL|SHIFT",    action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
  { key = "w",      mods = "CTRL|SHIFT",    action = act.CloseCurrentPane({ confirm = true }) },
  { key = "1",      mods = "CTRL|SHIFT",    action = act.ActivateTab(0) },
  { key = "2",      mods = "CTRL|SHIFT",    action = act.ActivateTab(1) },
  { key = "3",      mods = "CTRL|SHIFT",    action = act.ActivateTab(2) },
  { key = "4",      mods = "CTRL|SHIFT",    action = act.ActivateTab(3) },
  { key = "5",      mods = "CTRL|SHIFT",    action = act.ActivateTab(4) },
  { key = "6",      mods = "CTRL|SHIFT",    action = act.ActivateTab(5) },
  { key = "7",      mods = "CTRL|SHIFT",    action = act.ActivateTab(6) },
  { key = "8",      mods = "CTRL|SHIFT",    action = act.ActivateTab(7) },
  { key = "9",      mods = "CTRL|SHIFT",    action = act.ActivateTab(-1) },
  { key = "m",      mods = "CTRL|SHIFT",    action = act.Hide },
  { key = "x",      mods = "CTRL|SHIFT",    action = act.ActivateCommandPalette },
  { key = "e",      mods = "CTRL|SHIFT",    action = act.ShowDebugOverlay },
  { key = "Space",  mods = "CTRL|SHIFT",    action = act.ActivateCopyMode },
  { key = "y",      mods = "CTRL|SHIFT",    action = act.CopyTo("Clipboard") },
  { key = "p",      mods = "CTRL|SHIFT",    action = act.PasteFrom("Clipboard") },
  { key = "f",      mods = "CTRL|SHIFT",    action = act.Search("CurrentSelectionOrEmptyString") },
  { key = "h",      mods = "CTRL|SHIFT",    action = act.ActivatePaneDirection("Left") },
  { key = "j",      mods = "CTRL|SHIFT",    action = act.ActivatePaneDirection("Down") },
  { key = "k",      mods = "CTRL|SHIFT",    action = act.ActivatePaneDirection("Up") },
  { key = "l",      mods = "CTRL|SHIFT",    action = act.ActivatePaneDirection("Right") },
  { key = "Enter",  mods = "ALT",           action = act.ToggleFullScreen },
}

return config
