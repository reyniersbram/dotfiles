local require = require("util").cb_require
vim.api.nvim_create_augroup("", {})
return {
    {
        "williamboman/mason.nvim",
        config = require("plugins.config.lsp.mason"),
        cmd = {
            "Mason", "MasonInstall", "MasonLog",
            "MasonUninstall", "MasonUninstallAll", "MasonUpdate",
        },
    },
    {
        "williamboman/mason-lspconfig.nvim",
        dependencies = {
            "williamboman/mason.nvim",
        },
        config = require("plugins.config.lsp.mason-lspconfig"),
        event = { "LspAttach" },
    },
    {
        "neovim/nvim-lspconfig",
        dependencies = {
            "williamboman/mason.nvim",
            "williamboman/mason-lspconfig.nvim",
            "folke/neodev.nvim",
        },
        config = require("plugins.config.lsp.lspconfig"),
        event = { "BufReadPre", "BufNewFile" },
        cmd = { "LspInfo", "LspInstall", "LspUninstall" },
    },
    -- "jose-elias-alvarez/null-ls.nvim",
    -- "ray-x/lsp_signature.nvim"
    {
        "fladson/vim-kitty",
        ft = "kitty",
    },
    {
        "hinell/lsp-timeout.nvim",
        dependencies = {
            "neovim/nvim-lspconfig",
        },
        event = { "LspAttach" },
        config = require("plugins.config.lsp.timeout"),
    },

    -- TODO:
    -- b0o/schemastore.nvim
}
