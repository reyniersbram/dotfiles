-- Customized colors for in-app terminal
local terminal_colors = {
    -- normal colors
    "#000000", -- black
    "#cc0101", -- red
    "#4e9a06", -- green
    "#c5a100", -- yelow
    "#3565a5", -- blue
    "#74517b", -- magenta
    "#06989a", -- cyan
    "#d2d7ce", -- white
    -- bright colors
    "#545752", -- black
    "#ee2829", -- red
    "#8be334", -- green
    "#fde84f", -- yelow
    "#739ecf", -- blue
    "#ad7ea9", -- magenta
    "#35e3e2", -- cyan
    "#eeeeec", -- white
}

local defined_colors = 0
for _ in pairs(terminal_colors) do
    defined_colors = defined_colors + 1
end

if defined_colors == 16 then
    for index, value in ipairs(terminal_colors) do
        vim.g["terminal_color_" .. (index - 1)] = value
    end
end

-- Make floating windows appear transparent
vim.cmd.highlight({ "link" , "NormalFloat", "NormalNC" })
vim.cmd.highlight({ "link" , "FloatBorder", "NormalNC" })
