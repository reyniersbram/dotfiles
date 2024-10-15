local M = {}
M.adapter = function(callback, config)
    if config.request == "attach" then
        local port = (config.connect or config).port
        local host = (config.conenct or config).host or "127.0.0.1"
        callback {
            type = "server",
            port = assert(port, "`connect.port` is required for a python `attach` configuration"),
            host = host,
            options = { source_filetype = "python" },
        }
    else
        callback {
            type = "executable",
            command = vim.fn.exepath("debugpy-adapter"),
            options = { source_filetype = "python" },
        }
    end
end

M.configurations = {
    {
        type = "python",
        request = "launch",
        name = "Launch file",
        program = "${file}",
        pythonPath = function()
            -- if a virtual environment is active, the python
            -- executable of the environment will be used, else
            -- the global python installation will be used
            return vim.fn.exepath("python")
        end,
    },
}
return M
