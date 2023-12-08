local require = require("helpers.utils").cb_require

return {
    "goolord/alpha-nvim",
    dependencies = {
        "nvim-tree/nvim-web-devicons",
    },
    config = require("plugins.config.alpha"),
}
