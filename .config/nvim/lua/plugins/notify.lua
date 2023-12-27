local require = require("util").cb_require
return {
    {
        "rcarriga/nvim-notify",
        priority = 1000,
        config = require("plugins.config.notify"),
    },
}
