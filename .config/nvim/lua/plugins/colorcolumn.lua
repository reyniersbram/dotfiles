local require = require("util").cb_require

return {
    "Bekaboo/deadcolumn.nvim",
    config = require("plugins.config.deadcolumn"),
    event = { "InsertEnter" },
}
