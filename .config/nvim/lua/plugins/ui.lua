local require = require("util").cb_require

return {
    {
        "stevearc/dressing.nvim",
        config = require("plugins.config.dressing"),
        -- lazy = true,
    },
    {
        "nvim-tree/nvim-web-devicons",
        lazy = true,
    },
}
