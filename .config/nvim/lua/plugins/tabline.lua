local require = require("helpers.utils").require

return {
    "akinsho/bufferline.nvim",
    dependencies = {
        "moll/vim-bbye",
        "nvim-tree/nvim-web-devicons",
    },
    after = {
        "catppuccin",
    },
    config = require("plugins.config.bufferline"),
}
