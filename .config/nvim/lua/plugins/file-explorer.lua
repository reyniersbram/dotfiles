local require = require("helpers.utils").require

return {
    "nvim-tree/nvim-tree.lua",
    dependencies = {
        "nvim-tree/nvim-web-devicons",
    },
    config = require("plugins.config.nvim-tree"),
}

