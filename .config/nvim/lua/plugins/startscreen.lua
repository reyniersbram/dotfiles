local require = require("util").cb_require

return {
    "goolord/alpha-nvim",
    dependencies = {
        "nvim-tree/nvim-web-devicons",
    },
    config = require("plugins.config.alpha"),
}
