local require = require("util").cb_require

-- TODO: try LunarVim/breadcrumbs.nvim
return {
    "akinsho/bufferline.nvim",
    cond = false,
    dependencies = {
        "nvim-tree/nvim-web-devicons",
    },
    config = require("plugins.config.bufferline"),
}
