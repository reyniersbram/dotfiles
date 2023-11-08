local require = require("helpers.utils").require
return {
    {
        "williamboman/mason.nvim",
        config = require("plugins.config.lsp.mason"),
    },
    {
        "williamboman/mason-lspconfig.nvim",
        dependencies = {
            "williamboman/mason.nvim",
        },
        config = require("plugins.config.lsp.mason-lspconfig"),
    },
    {
        "neovim/nvim-lspconfig",
        dependencies = {
            "williamboman/mason.nvim",
            "williamboman/mason-lspconfig.nvim",
        },
        config = require("plugins.config.lsp.lspconfig"),
        -- event = { "BufReadPost", "BufNewFile" },
        -- cmd = { "LspInfo", "LspInstall", "LspUninstall" },
    },
    -- "jose-elias-alvarez/null-ls.nvim",
    -- "ray-x/lsp_signature.nvim"
    -- "RRethy/vim-illuminate"
    -- "lvimuser/lsp-inlayhints.nvim"
    "fladson/vim-kitty",
    {
        "hinell/lsp-timeout.nvim",
        dependencies = {
            "neovim/nvim-lspconfig",
        },
        config = require("plugins.config.lsp.timeout"),
    },
}
