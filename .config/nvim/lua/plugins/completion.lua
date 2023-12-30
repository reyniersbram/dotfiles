local require = require("util").cb_require

return {
    {
        "hrsh7th/nvim-cmp",
        config = require("plugins.config.cmp"),
        event = { "InsertEnter" },
        dependencies = {
            -- sources
            "hrsh7th/cmp-buffer", -- buffer completions
            "hrsh7th/cmp-path", -- path completions
            "hrsh7th/cmp-cmdline", -- cmdline completions
            "saadparwaiz1/cmp_luasnip", -- snippet completions
            "hrsh7th/cmp-nvim-lsp", -- integration with lsp
            "hrsh7th/cmp-nvim-lua", -- extra nvim lua completion
            {
                "hrsh7th/cmp-emoji", -- emojiiis🙂
                cond = false,
            },
            {
                "f3fora/cmp-spell", -- spell checker
                cond = false,
            },

            -- snippets
            "L3MON4D3/LuaSnip", --snippet engine
            "rafamadriz/friendly-snippets", -- a bunch of snippets to use
        },
    },
}
