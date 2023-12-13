local M = {}

M.ssh2http =
    -- Converts an ssh url, formatted like `git@some.domain:user/repo.git`
    -- to an http(s) url formatted like `https://some.domain/user/repo` if needed
    --- @param url string url to be converted
    --- @return string | nil # the formatted string
    function(url)
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

--- Try to execute a function using a plugin.
--- @param plugin_name string
--- @param callback fun(plugin)
--- @param opts? table
--- @return boolean # true if the function was executed, else false
function M.try_with_plugin(plugin_name, callback, opts)
    -- TODO: use opts to set verbose
    local plugin_installed, plugin = pcall(require, plugin_name)
    if plugin_installed then
        callback(plugin)
        return true
    end
    return false
end

return M
