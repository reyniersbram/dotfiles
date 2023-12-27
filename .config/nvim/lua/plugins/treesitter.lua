local require = require("util").cb_require

return {
    {
        "nvim-treesitter/nvim-treesitter",
        dependencies = {
            "JoosepAlviste/nvim-ts-context-commentstring",
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
    },
    {
        "windwp/nvim-autopairs",
        config = require("plugins.config.autopairs"),
        event = { "InsertEnter" },
    },
}
