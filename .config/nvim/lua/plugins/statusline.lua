local require = require("helpers.utils").require

return {
    "nvim-lualine/lualine.nvim",
    dependencies  = {
        "nvim-tree/nvim-web-devicons",
        "arkav/lualine-lsp-progress",
    },
    config = require("plugins.config.statusline")
}
