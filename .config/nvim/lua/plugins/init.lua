local require = require("util").cb_require

-- TODO:
-- try neodev.nvim

return {
    -- Social
    {
        "andweeb/presence.nvim",
        config = require("plugins.config.presence"),
        cond = false,
    },
    "wakatime/vim-wakatime",

    -- Misc
    {
        "nvim-lua/popup.nvim", -- An implementation of the Popup API from vim in Neovim
        lazy = true,
    },
    {
        "nvim-lua/plenary.nvim", -- Useful lua functions used by lots of plugins
        lazy = true,
    },

    -- Markdown preview plugin
    {
        "iamcco/markdown-preview.nvim",
        build = "cd app && yarn install",
        ft = "markdown",
    },

    -- Color Highlighting
    {
        "norcalli/nvim-colorizer.lua",
        config = require("config.colorizer"),
        cond = false,
    },
}
