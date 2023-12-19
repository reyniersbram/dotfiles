local require = require("helpers.utils").cb_require

return {
    {
        "numToStr/Comment.nvim",
        dependencies = {
            "JoosepAlviste/nvim-ts-context-commentstring",
        },
        config = require("plugins.config.comment"),
        event = { "BufReadPre", "BufNewFile" },
    },
    {
        "folke/todo-comments.nvim",
        dependencies = {
            "nvim-lua/plenary.nvim",
            "nvim-telescope/telescope.nvim",
        },
        config = require("plugins.config.todo"),
        event = { "BufReadPre", "BufNewFile" },
    },
}
