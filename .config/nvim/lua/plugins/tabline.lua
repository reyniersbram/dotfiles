local require = require("util").cb_require

return {
    {
        -- TODO: try nvim-navbuddy
        "SmiteshP/nvim-navic",
        config = require("plugins.config.navic"),
        -- TODO: lazyloading does not always work
        -- event = { "LspAttach" },
    },
    -- {
    --     "akinsho/bufferline.nvim",
    --     cond = false,
    --     dependencies = {
    --         "nvim-tree/nvim-web-devicons",
    --     },
    --     config = require("plugins.config.bufferline"),
    -- },
}
