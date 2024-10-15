local require = require("util").cb_require

return {
    {
        "nvim-lualine/lualine.nvim",
        dependencies = {
            "nvim-tree/nvim-web-devicons",
            "AndreM222/copilot-lualine",
            "linrongbin16/lsp-progress.nvim",
        },
        config = require("plugins.config.statusline"),
    },
    {
        "linrongbin16/lsp-progress.nvim",
        config = require("plugins.config.statusline.lsp-progress"),
    },
}
