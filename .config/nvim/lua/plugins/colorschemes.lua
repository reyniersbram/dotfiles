local require = require("helpers.utils").require

return {
    {
        "ellisonleao/gruvbox.nvim",
        lazy = true,
        priority = 100,
    },
    {
        "navarasu/onedark.nvim",
        lazy = false,
        priority = 100,
        config = require("plugins.config.colorschemes.onedark"),
    },
}
