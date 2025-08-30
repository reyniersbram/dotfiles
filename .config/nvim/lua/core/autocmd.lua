local M = {}

M.user_autocmd_group = vim.api.nvim_create_augroup("UserConfig", { clear = true })

M.default_opts = {
    group = M.user_autocmd_group,
}

---@param events string|string[]
---@param user_opts vim.api.keyset.create_autocmd
---@return integer
--- Wrapper for creating autocmds, autocmd will be in the UserConfig group
function M.create_autocmd(events, user_opts)
    local opts = vim.tbl_deep_extend("force", user_opts, M.default_opts)
    return vim.api.nvim_create_autocmd(events, opts)
end

M.create_autocmd("TextYankPost", {
    desc = "Shortly highlight yanked text",
    callback = function()
        vim.hl.on_yank {
            timeout = 100,
            on_macro = false,
            on_visual = true,
        }
    end,
})

M.create_autocmd("VimResized", {
    desc = "Auto resize splits on terminal resize",
    command = "wincmd =",
})

return M
