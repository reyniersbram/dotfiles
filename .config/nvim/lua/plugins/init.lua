local require = require("helpers.utils").require
return {
    -- LSP
    {
        "williamboman/mason.nvim",
        config = require("plugins-config.lsp.mason"),
    },
    {
        "williamboman/mason-lspconfig.nvim",
        dependencies = {
            "williamboman/mason.nvim",
        },
        config = require("plugins-config.lsp.mason-lspconfig"),
    },
    {
        "neovim/nvim-lspconfig",
        dependencies = {
            "williamboman/mason.nvim",
            "williamboman/mason-lspconfig.nvim",
        },
        config = require("plugins-config.lsp.lspconfig"),
        -- event = { "BufReadPost", "BufNewFile" },
        -- cmd = { "LspInfo", "LspInstall", "LspUninstall" },
    },
    -- "jose-elias-alvarez/null-ls.nvim",
    -- "ray-x/lsp_signature.nvim"
    -- "RRethy/vim-illuminate"
    -- "lvimuser/lsp-inlayhints.nvim"
    "fladson/vim-kitty",

    -- Social
    -- "wakatime/vim-wakatime",

    -- Notifications
    "rcarriga/nvim-notify",

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

    -- Lualine
    {
        "nvim-lualine/lualine.nvim",
        dependencies  = {
            "nvim-tree/nvim-web-devicons",
            "arkav/lualine-lsp-progress",
        },
        config = require("config.statusline")
    },

    -- Nvim-Navic
    {
        "SmiteshP/nvim-navic",
        dependencies = {
            "neovim/nvim-lspconfig",
        },
    },
}
