local M = {}

local function setup_auto_format(event)
    local auto_format_group_name = "LspAutoFormat"
    function M.enable_auto_format()
        local group = vim.api.nvim_create_augroup(
            auto_format_group_name,
            { clear = true }
        )
        vim.api.nvim_create_autocmd("BufWritePre", {
            buffer = event.buf,
            group = group,
            desc = "Format on Save",
            callback = function(e)
                vim.lsp.buf.format {
                    bufnr = e.buf,
                    async = false,
                    timeout_ms = 10000,
                }
            end
        })
        vim.notify "Automatic formatting enabled"
    end

    function M.disable_auto_format()
        require("util").remove_augroup(auto_format_group_name)
        vim.notify "Automatic formatting disabled"
    end

    function M.toggle_auto_format()
        if vim.fn.exists("#" .. auto_format_group_name .. "#BufWritePre") == 0 then
            M.enable_auto_format()
        else
            M.disable_auto_format()
        end
    end

    vim.api.nvim_create_user_command(
        "LspToggleAutoFormat",
        function()
            M.toggle_auto_format()
        end,
        {
            nargs = 0,
            desc = "Toggle automatic formatting on save",
        }
    )

    vim.api.nvim_create_user_command(
        "LspEnableAutoFormat",
        function()
            M.enable_auto_format()
        end,
        {
            nargs = 0,
            desc = "Enable automatic formatting on save",
        }
    )

    vim.api.nvim_create_user_command(
        "LspDisableAutoFormat",
        function()
            M.disable_auto_format()
        end,
        {
            nargs = 0,
            desc = "Disable automatic formatting on save",
        }
    )
end

vim.api.nvim_create_autocmd("LspAttach", {
    desc = "Setup automatic formatting",
    callback = function(event) setup_auto_format(event) end,
})

vim.notify("autoformatting")

return M
