local M = {}

---@param host string
---@param port integer
---@return dap.Adapter
local function delve_adapter_config(host, port)
    local addr = host .. ":" .. port
    return {
        host = host,
        port = port,
        type = "server",
        executable = {
            command = "dlv",
            args = { "dap", "-l", addr, "--log", "--log-output=dap" },
            detached = vim.fn.has("win32") == 0,
            cwd = nil,
        },
        options = {
            max_retries = 14,
            initialize_timeout_sec = 4,
            disconnect_timeout_sec = 3,
        },
    }
end

---@class dap.Configuration
---@field port? integer
---@field host? string

---@type dap.AdapterFactory
function M.adapter(callback, config, parent)
    local host = config.host
    if host == nil then
        host = "127.0.0.1"
    end
    local port = config.port
    if port == nil then
        port = require("util.tcp").get_free_port(host)
    end
    local adapter = delve_adapter_config(host, port)
    callback(adapter)
end

-- see https://pkg.go.dev/github.com/go-delve/delve/service/dap#LaunchAttachCommonConfig
---@class dap.go.Configuration : dap.Configuration
---@field request "launch" | "attach"
---@field followExec? boolean
---@field followExecRegex? string
---@field stopOnEntry? boolean
---@field backend? "default" | "native" | "lldb" | "rr"
---@field stackTraceDepth? integer
---@field showGlobalVariables? boolean
---@field showRegisters? boolean
---@field hideSystemGoroutines? boolean
---@field goroutineFilters? string
---@field showPprofLabels? string[]
---@field substitutePath? any[]

-- see https://pkg.go.dev/github.com/go-delve/delve/service/dap#LaunchConfig
---@class dap.go.LaunchConfiguration : dap.go.Configuration
---@field request "launch"
---@field mode "debug" | "test" | "exec" | "replay" | "core"
---@field program string
---@field args? string[]
---@field cwd? string
---@field buildFlags? string | string[]
---@field output? string
---@field noDebug? boolean
---@field traceDirPath? string
---@field coreFilePath? string
---@field dlvCwd? string
---@field env? table<string, string>
---@field outputMode string
---@field stdinFrom? string
---@field stdoutTo? string
---@field stderrTo? string

-- see https://pkg.go.dev/github.com/go-delve/delve/service/dap#AttachConfig
---@class dap.go.AttachConfiguration : dap.go.Configuration
---@field request "attach"
---@field mode "local" | "remote"
---@field processId? integer
---@field waitFor? string
---@field guessSubstitutePath? any


-- see https://github.com/leoluz/nvim-dap-go/blob/main/lua/dap-go.lua
---@type (dap.go.LaunchConfiguration | dap.go.AttachConfiguration)[]
M.configurations = {
    {
        type = "delve",
        name = "Debug file",
        request = "launch",
        mode = "debug",
        program = "${file}",
        outputMode = "remote",
    },
    {
        type = "delve",
        name = "Debug (with args)",
        request = "launch",
        mode = "debug",
        program = "${file}",
        outputMode = "remote",
        args = require("plugins.dap.util").get_input_list("Args: "),
    },
    {
        type = "delve",
        name = "Debug (with input)",
        request = "launch",
        mode = "debug",
        program = "${file}",
        outputMode = "remote",
        stdinFrom = "input.txt"
    },
    {
        type = "delve",
        name = "Debug test (go.mod)",
        request = "launch",
        mode = "test",
        program = "./${relativeFileDirname}",
        outputMode = "remote",
    },
    -- Attaches to a running go program
    {
        type = "delve",
        name = "Debug Attach",
        request = "attach",
        mode = "local",
        processId = function()
            local opts = {}
            vim.ui.input(
                { prompt = "Filter process name: " },
                function(choice)
                    opts.filter = choice or ""
                end
            )
            return require("dap.utils").pick_process(opts)
        end
    },
}

return M
