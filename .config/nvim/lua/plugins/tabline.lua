local require = require("helpers.utils").require

return {
    "akinsho/bufferline.nvim",
    dependencies = {
        "moll/vim-bbye",
        "nvim-tree/nvim-web-devicons",
    },
    config = require("plugins.config.bufferline"),
}
