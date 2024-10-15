return {
    {
        "mfussenegger/nvim-dap",
        lazy = true,
        dependencies = {
            -- TODO: Lazy load this
            "williamboman/mason.nvim",
        },
        config = function()
            require("util").try_with_module("dap", function(dap)
                local configurations = { "python" }
                for _, language in ipairs(configurations) do
                    local lang_settings = require("util.dap." .. language)
                    dap.adapters[language] = lang_settings.adapter
                    dap.configurations.python = lang_settings.configurations
                end
            end)
        end,
    },
    {
        "rcarriga/nvim-dap-ui",
        lazy = true,
        dependencies = {
            "mfussenegger/nvim-dap",
            "folke/neodev.nvim",
        },
        config = function()
            require("util").try_with_module("neodev", function(neodev)
                neodev.setup {
                    library = { plugins = { "nvim-dap-ui" }, types = true }
                }
            end)
            require("dapui").setup {

            }
        end,
    },
}
