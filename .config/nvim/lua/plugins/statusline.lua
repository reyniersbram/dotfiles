local require = require("helpers.utils").cb_require

return {
    -- Nvim-Navic
    {
        "SmiteshP/nvim-navic",
        dependencies = {
            "neovim/nvim-lspconfig",
        },
    },
    {
        "nvim-lualine/lualine.nvim",
        dependencies  = {
            "nvim-tree/nvim-web-devicons",
        },
        config = require("plugins.config.statusline")
    },
}
