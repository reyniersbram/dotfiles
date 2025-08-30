---Get input from the user
---@param prompt string
---@param default string
---@param format? fun(choice: string): any
---@return fun(): thread
local function get_input_callback(prompt, default, format)
    if format == nil then
        format = function(c)
            return c
        end
    end
    return function()
        return coroutine.create(function(dap_run)
            vim.ui.input({
                prompt = prompt,
                default = default,
            }, function(choice)
                coroutine.resume(dap_run, format(choice))
            end)
        end)
    end
end

return {
    "mfussenegger/nvim-dap",
    config = function()
        local dap = require("dap")
        local icons = require("util.icons").ui.debug

        vim.fn.sign_define(
            "DapBreakpoint",
            {
                text = icons.breakpoint.breakpoint,
                texthl = "",
                linehl = "",
                numhl = "",
            }
        )
        vim.fn.sign_define(
            "DapBreakpointCondition",
            {
                text = icons.breakpoint.condition,
                texthl = "",
                linehl = "",
                numhl = "",
            }
        )
        vim.fn.sign_define(
            "DapLogPoint",
            {
                text = icons.breakpoint.log,
                texthl = "",
                linehl = "",
                numhl = "",
            }
        )
        vim.fn.sign_define(
            "DapStopped",
            {
                text = icons.breakpoint.stopped,
                texthl = "",
                linehl = "",
                numhl = "",
            }
        )
        vim.fn.sign_define(
            "DapBreakpointRejected",
            {
                text = icons.breakpoint.rejected,
                texthl = "",
                linehl = "",
                numhl = "",
            }
        )

        dap.defaults.fallback = {
            exception_breakpoints = "default",
            stepping_granularity = "statement",
            terminal_win_cmd = "belowright vnew",
            focus_terminal = true,
            auto_continue_if_many_stopped = true,
            switchbuf = "usevisible," .. vim.o.switchbuf,
        }

        ---@class dap.Configuration
        ---@field port? integer|fun(): thread
        ---@field host? string

        dap.adapters.delve = function(
            callback,
            config,
            parent
        )
            local host = config.host
            if host == nil then
                host = "127.0.0.1"
            end
            local port = config.port
            if port == nil then
                port = require("util.tcp").get_free_port(host)
            end
            local addr = host .. ":" .. port
            vim.print(addr)

            callback {
                type = "server",
                port = port,
                host = host,
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

        dap.configurations.go = {
            {
                name = "Debug file",
                type = "delve",
                request = "launch",
                mode = "debug",
                program = "${file}",
                outputMode = "remote",
            },
            {
                type = "delve",
                name = "Debug (with args)",
                request = "launch",
                program = "${file}",
                outputMode = "remote",
                args = get_input_callback("Args: ", "", function(choice)
                    return vim.split(choice or "", "%s+", { trimempty = true })
                end)
            },
            -- TODO(https://github.com/go-delve/delve/issues/4093)
            -- {
            --     type = "delve",
            --     name = "Debug (with input)",
            --     request = "launch",
            --     program = "${file}",
            --     outputMode = "remote",
            --     dlvFlags = { "-r", "stdin:in.txt" },
            -- },
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

        dap.adapters.python = {
            type = "executable",
            command = "python",
            args = { "-m", "debugpy.adapter" },
        }

        dap.configurations.python = {
            {
                type = "python",
                request = "launch",
                name = "Debug",
                program = "${file}",
                console = "integratedTerminal",
            },
        }
    end
}
