local require = require("util").cb_require

return {
    "nvim-telescope/telescope.nvim",
    branch = '0.1.x',
    dependencies = {
        "nvim-lua/plenary.nvim",
        {
            "nvim-telescope/telescope-fzf-native.nvim", -- Better telescope sorting
            build = "make"
        },
        {
            "nvim-telescope/telescope-media-files.nvim",
            enabled = false,
        },
    },
    config = require("plugins.config.telescope"),
}
