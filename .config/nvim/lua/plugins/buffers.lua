local require = require("util").cb_require
return {
    "moll/vim-bbye",
    config = require("plugins.config.buffers"),
    keys = { "<C-w>" }
}
