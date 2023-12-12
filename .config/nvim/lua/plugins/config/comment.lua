local status_ok, comment = pcall(require, "Comment")
if not status_ok then
    vim.notify("Comment plugin not found!")
    return
end

comment.setup {
    padding = true,
    sticky = true,
    ignore = "^$",
    toggler = {
        line = "gcc",
        block = "gbc",
    },
    opleader = {
        line = "gc",
        block = "gb",
    },
    extra = {
        above = "gcO",
        below = "gco",
        eol = "gcA",
    },
    mappings = {
        basic = true,
        extra = true,
    },
    pre_hook = require("ts_context_commentstring.integrations.comment_nvim").create_pre_hook(),
    post_hook = nil,
}
