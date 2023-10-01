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

    -- ColorColumn
    "Bekaboo/deadcolumn.nvim",

    -- Social
    "andweeb/presence.nvim",
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

    -- Telescope
    {
        "nvim-telescope/telescope.nvim",
        tag = '0.1.0',
        dependencies = {
            "nvim-lua/plenary.nvim",
            {
               "nvim-telescope/telescope-fzf-native.nvim", -- Better telescope sorting
                build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build"
            },
        },
    },
    -- "nvim-telescope/telescope-media-files.nvim",

    -- Color Highlighting
    "norcalli/nvim-colorizer.lua",

    -- Treesitter
    {
        "nvim-treesitter/nvim-treesitter",
        build = ":TSUpdate",
    },
    "JoosepAlviste/nvim-ts-context-commentstring",

    -- Autopairs
    "windwp/nvim-autopairs",

    -- Commenting
    "numToStr/Comment.nvim",

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
