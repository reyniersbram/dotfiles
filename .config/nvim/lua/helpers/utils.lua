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

function M.require(module_name)
    return function()
        require(module_name)
    end
end

return M
