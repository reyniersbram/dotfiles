vim.diagnostic.config {
    underline = {
        severity = { min = vim.diagnostic.severity.HINT },
    },
    virtual_text = {
        severity = { min = vim.diagnostic.severity.WARN },
        current_line = false,
        source = false,
        spacing = 4,
        prefix = function(diagnostic)
            return require("util.icons").diagnostics[diagnostic.severity]
        end,
        suffix = function(diagnostic)
            return string.format(" [%s]", tostring(diagnostic.code))
        end,
        format = function(diagnostic)
            return diagnostic.message
        end,
        hl_mode = "combine",
        virt_text_pos = "eol",
    },
    virtual_lines = false,
    signs = {
        severity = { min = vim.diagnostic.severity.HINT },
        priority = 10,
        text = require("util.icons").diagnostics,
    },
    float = {
        scope = "line",
        severity_sort = true,
        severity = { min = vim.diagnostic.severity.HINT },
        source = true,
    },
    update_in_insert = true,
    severity_sort = true,
    jump = {
        float = false,
        wrap = true,
    },
}
