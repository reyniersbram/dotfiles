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

        local dap_go = require("plugins.dap.go")
        dap.adapters.delve = dap_go.adapter
        dap.configurations.go = dap_go.configurations

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
