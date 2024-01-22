local require = require("util").cb_require

return {
    {
        "nvim-treesitter/nvim-treesitter",
        dependencies = {
            "nvim-treesitter/nvim-treesitter-textobjects",
            "windwp/nvim-ts-autotag",
        },
        build = ":TSUpdate",
        config = require("plugins.config.treesitter"),
        cmd = { "TSInstall", "TSBufEnable", "TSBufDisable", "TSModuleInfo" },
        event = { "BufReadPre", "BufNewFile" }
    },
    {
        "JoosepAlviste/nvim-ts-context-commentstring",
        opts = {
            enable_autocmd = false,
        },
        lazy = true,
    },
    {
        "nvim-treesitter/nvim-treesitter-textobjects",
        dependencies = {
            "nvim-treesitter/nvim-treesitter",
        },
        config = require("plugins.config.treesitter-textobjects"),
        lazy = true,
    },
    {
        "windwp/nvim-autopairs",
        config = require("plugins.config.autopairs"),
        event = { "InsertEnter" },
    },
}
