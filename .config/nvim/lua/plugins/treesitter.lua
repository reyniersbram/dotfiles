local require = require("helpers.utils").cb_require

return {
    {
        "nvim-treesitter/nvim-treesitter",
        dependencies = {
            "JoosepAlviste/nvim-ts-context-commentstring",
        },
        build = ":TSUpdate",
        config = require("plugins.config.treesitter")
    },
    {
        "JoosepAlviste/nvim-ts-context-commentstring",
        opts = {
            enable_autocmd = false,
        }
    },
}
