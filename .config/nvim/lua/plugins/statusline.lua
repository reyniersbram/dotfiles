local require = require("helpers.utils").cb_require

return {
    {
        "SmiteshP/nvim-navic",
        -- TODO: figure out how to lazy load this
        -- https://github.com/LazyVim/LazyVim/blob/879e29504d43e9f178d967ecc34d482f902e5a91/lua/lazyvim/plugins/extras/editor/navic.lua#L6
        -- dependencies = {
        --     "neovim/nvim-lspconfig",
        -- },
        event = { "LspAttach" },
        config = require("plugins.config.statusline.navic"),
    },
    {
        "nvim-lualine/lualine.nvim",
        dependencies  = {
            "nvim-tree/nvim-web-devicons",
        },
        config = require("plugins.config.statusline")
    },
}
