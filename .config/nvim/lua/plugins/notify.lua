local require = require("helpers.utils").cb_require
return {
    {
        "rcarriga/nvim-notify",
        priority = 1000,
        config = require("plugins.config.notify"),
        dependencies = {
            "nvim-telescope/telescope.nvim",
        },
    },
}
