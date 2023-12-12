local status_ok, todo = pcall(require, "todo-comments")
if not status_ok then
    vim.notify("Todo Comments not found")
    return
end

local icons = require("helpers.icons")

-- TODO: some todo
-- FIXME: some fixme
-- NOTE: some note
-- HACK: some hack
-- WARN: some warning
-- TEST: some test
-- PERF: some performance
-- NOIMP: no implementation
todo.setup {
    signs = true,
    sign_priority = 8,
    colors = {
        error = { "DiagnosticError", "ErrorMsg", "#DC2626" },
        warn = { "DiagnosticWarn", "WarningMsg", "#FBBF24" },
        info = { "DiagnosticInfo", "#2563EB" },
        hint = { "DiagnosticHint", "#10B981" },
        default = { "Identifier", "#7C3AED" },
        test = { "Identifier", "#FF00FF" }
    },
    keywords = {
        TODO = {
            icon = icons.todo.TODO,
            color = "info",
        },
        FIXME = {
            icon = icons.todo.FIXME,
            color = "error",
            alt = { "FIX", "BUG", "ISSUE" },
        },
        NOTE = {
            icon = icons.todo.NOTE,
            color = "hint",
            alt = { "INFO" },
        },
        HACK = {
            icon = icons.todo.HACK,
            color = "warn",
        },
        WARN = {
            icon = icons.todo.WARN,
            color = "warn",
        },
        TEST = {
            icon = icons.todo.TEST,
            color = "test",
        },
        PERF = {
            icon = icons.todo.PERF,
            color = "test",
        },
        NOIMP = {
            icon = icons.todo.NOIMP,
            color = "warn",
        }
    },
    gui_style = {
        fg = "NONE",
        bg = "BOLD",
    },
    merge_keywords = true,
    highlight = {
        multiline = false,
        multiline_pattern = "^.",
        multiline_context = 10,
        before = "fg",
        keyword = "wide",
        after = "fg",
        pattern = [[.*<(KEYWORDS)\s*:]],
        comments_only = true,
        max_line_len = 400,
        exclude = {},
    },
    search = {
        command = "rg",
        args = {
            "--color=never",
            "--no-heading",
            "--with-filename",
            "--line-number",
            "--column",
        },
        pattern = [[\b(KEYWORDS):]]
    },
}

-- TODO: should not be here
vim.keymap.set("n", "gtn", todo.jump_next, { desc = "Next todo comment" })
vim.keymap.set("n", "gtp", todo.jump_prev, { desc = "Previous todo comment" })
