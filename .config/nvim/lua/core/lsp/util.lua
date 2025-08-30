local M = {}

--- Return names of LSP clients, defaults to all active clients.
---
---@param clients? vim.lsp.Client[] The clients for which the names are required
---@return Iter<string>
function M.client_names(clients)
    return vim.iter(clients or vim.lsp.get_clients())
        ---@param client vim.lsp.Client
        ---@return string
        :map(function(client)
            return client.name
        end)
end

function M.complete_client(arg)
    return M.client_names()
        ---@param name string
        :filter(function(name)
            return name:sub(1, #arg) == arg
        end)
        :totable()
end

function M.get_typescript_server_path(root_dir)
    local project_roots = vim.fs.find(
        "node_modules",
        { path = root_dir, upward = true, limit = math.huge }
    )
    for _, project_root in ipairs(project_roots) do
        local typescript_path = project_root .. "/typescript"
        vim.print(typescript_path)
        local stat = vim.loop.fs_stat(typescript_path)
        if stat and stat.type == "directory" then
            return typescript_path .. "/lib"
        end
    end
    return ""
end

return M
