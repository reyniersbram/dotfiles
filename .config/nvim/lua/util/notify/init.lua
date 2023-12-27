local M = {}

--- Inform the user a module could not be found, uses vim.notify.
--- @param modname string
function M.notify_not_found(modname)
    vim.notify(
        modname,
        vim.log.levels.WARN,
        {
            title = "Could not find module",
            render = "compact",
            timeout = 2500,
        }
    )
end

return M
