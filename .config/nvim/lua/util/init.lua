local M = {}

--- Converts an ssh url, formatted like `git@some.domain:user/repo.git`
--- to an http(s) url formatted like `https://some.domain/user/repo` if needed
--- @param url string
--- @return string | nil
function M.ssh2http(url)
    if url == nil then
        return nil
    end
    if string.sub(url, 1, 4) == "http" then
        return url
    end
    if string.sub(url, 1, 4) == "git@" then
        local length = string.len(url)
        url = "https://" ..
            string.gsub(
                string.sub(url, 5, length - 4),
                ":", "/", 1
            )
        return url
    end
    return nil
end

--- Same as require, but returns a callback to require a certain module
--- @param modname string
--- @return fun()
function M.cb_require(modname)
    return function()
        require(modname)
    end
end

--- Try to execute a given callback with a module. Does nothing when module
--- could not be found
--- @param modname string
--- @param callback fun(plugin)
--- @param opts? table
--- @return boolean # true if the function was executed, else false
function M.try_with_module(modname, callback, opts)
    opts = vim.tbl_deep_extend("keep", opts or {}, {
        verbose = true,
    })
    local plugin_installed, plugin = pcall(require, modname)
    if plugin_installed then
        callback(plugin)
        return true
    end
    if opts.verbose then
        require("util.notify").notify_not_found(modname)
    end
    return false
end

--- Remove an augroup, if it exists. Returns false if augroup didn't exist.
--- @param name string
--- @return boolean
function M.remove_augroup(name)
    if vim.fn.exists("#" .. name) == 1 then
        vim.api.nvim_clear_autocmds({ group = name })
        return true
    end
    return false
end

return M
