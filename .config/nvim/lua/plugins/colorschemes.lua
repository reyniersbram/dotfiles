local require = require("helpers.utils").require

return {
    {
        "ellisonleao/gruvbox.nvim",
        lazy = true,
        priority = 100,
    },
    {
        "navarasu/onedark.nvim",
        lazy = true,
        priority = 100,
        config = require("plugins.config.colorschemes.onedark"),
    },
    {
        "catppuccin/nvim",
        name = "catppuccin",
        lazy = false,
        priority = 100,
        config = require("plugins.config.colorschemes.catppuccin"),
    },
}
