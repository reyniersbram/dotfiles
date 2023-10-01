local function config()
    local status_ok, comment = pcall(require, "Comment")
    if not status_ok then
        vim.notify("Comment plugin not found!")
        return
    end

    comment.setup {
        pre_hook = require("ts_context_commentstring.integrations.comment_nvim").create_pre_hook(),
    }

    local ft_status_ok, ft_comment = pcall(require, "Comment.ft")
    if not ft_status_ok then
        vim.notify("Comment.ft not found!")
        return
    end

    ft_comment.lua = {"-- %s", "--[[\n%s\n-- ]]"}
end

return {
    "numToStr/Comment.nvim",
    config = config,
}
