local require = require("helpers.utils").require
return {
    -- Social
    {
        "andweeb/presence.nvim",
        config = require("plugins.config.presence"),
    },
    "wakatime/vim-wakatime",

    -- Notifications
    {
        "rcarriga/nvim-notify",
        priority = 1000,
    },

    -- Misc
    "nvim-lua/popup.nvim", -- An implementation of the Popup API from vim in Neovim
    "nvim-lua/plenary.nvim", -- Useful lua functions used by lots of plugins

    -- Markdown preview plugin
    {
        "iamcco/markdown-preview.nvim",
        build = "cd app && npm install",
        ft = "markdown",
    },


    -- Color Highlighting
    {
        "norcalli/nvim-colorizer.lua",
        config = require("config.colorizer")
    },

    -- Autopairs
    {
        "windwp/nvim-autopairs",
        config = require("config.autopairs")
    },

    -- Git
    {
        "lewis6991/gitsigns.nvim",
        config = require("config.gitsigns")
    },

    -- Nvim-Navic
    {
        "SmiteshP/nvim-navic",
        dependencies = {
            "neovim/nvim-lspconfig",
        },
    },
}
