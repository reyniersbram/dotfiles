return {
    -- LSP
    "neovim/nvim-lspconfig",
    "williamboman/mason.nvim",
    {
        "williamboman/mason-lspconfig.nvim",
        dependencies = {
            "williamboman/mason.nvim",
            "neovim/nvim-lspconfig",
        }
    },
    -- "jose-elias-alvarez/null-ls.nvim",
    -- "ray-x/lsp_signature.nvim"
    -- "RRethy/vim-illuminate"
    -- "lvimuser/lsp-inlayhints.nvim"
    "fladson/vim-kitty",

    -- Social
    "wakatime/vim-wakatime",






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
    "norcalli/nvim-colorizer.lua",

    -- Autopairs
    "windwp/nvim-autopairs",

    -- Git
    "lewis6991/gitsigns.nvim",

    -- Lualine
    {
        "nvim-lualine/lualine.nvim",
        dependencies  = {
            "nvim-tree/nvim-web-devicons",
            "arkav/lualine-lsp-progress",
        },
    },

    -- Nvim-Navic
    {
        "SmiteshP/nvim-navic",
        dependencies = {
            "neovim/nvim-lspconfig",
        },
    },
}
