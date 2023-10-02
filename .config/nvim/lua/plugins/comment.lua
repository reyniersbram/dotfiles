local require = require("helpers.utils").require

return {
    "numToStr/Comment.nvim",
    dependencies = {
        "JoosepAlviste/nvim-ts-context-commentstring",
    },
    config = require("plugins.config.comment"),
}
