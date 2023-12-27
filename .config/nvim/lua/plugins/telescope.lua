local require = require("util").cb_require

return {
    "nvim-telescope/telescope.nvim",
    branch = '0.1.x',
    dependencies = {
        "nvim-lua/plenary.nvim",
        {
            "nvim-telescope/telescope-fzf-native.nvim", -- Better telescope sorting
            build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build"
        },
-- "nvim-telescope/telescope-media-files.nvim",
    },
    config = require("plugins.config.telescope"),
}
