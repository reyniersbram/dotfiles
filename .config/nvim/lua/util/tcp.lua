local M = {}

---Search for a free TCP port
---@param host string
---@return integer
function M.get_free_port(host)
    local tcp = assert(vim.uv.new_tcp(), "Must be able to create tcp client")
    tcp:bind(host, 0)
    local port = tcp:getsockname().port
    tcp:shutdown()
    tcp:close()
    return port
end

return M
