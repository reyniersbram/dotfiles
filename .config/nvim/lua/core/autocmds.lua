local user_group = vim.api.nvim_create_augroup("UserConfig", { clear = true })
vim.api.nvim_create_autocmd({ "TextYankPost" }, {
    group = user_group,
    desc = "Shortly highlight yanked text",
    callback = function()
        vim.highlight.on_yank { higroup = "Visual", timeout = 100 }
    end,
})
