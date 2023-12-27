local require = require("util").cb_require
return {
    {
        "lewis6991/gitsigns.nvim",
        config = require("plugins.config.gitsigns"),
    },
    -- TODO: check lazygit.nvim and octo.nvim
}
