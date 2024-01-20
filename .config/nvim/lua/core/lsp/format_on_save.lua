local M = {}

local plugin_name = "FormatOnSave"
local auto_format_group_name = "LspFormatOnSave"

local function setup_auto_format(event)
    function M.enable_auto_format()
        local client_id = vim.tbl_get(event, "data", "client_id")
        local client = client_id and vim.lsp.get_client_by_id(client_id)
        if client == nil then
            vim.notify(
                "Could not setup format-on-save, no client found",
                vim.log.levels.WARN,
                { title = plugin_name }
            )
            return
        elseif not client.supports_method("textDocument/formatting") then
            vim.notify(
                "Could not setup format-on-save, "
                .. client.name .. " does not support formatting",
                vim.log.levels.WARN,
                { title = plugin_name }
            )
            return
        end
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
        vim.notify(
            "Formatting on save enabled",
            vim.log.levels.INFO,
            { title = plugin_name }
        )
    end

    function M.disable_auto_format()
        require("util").remove_augroup(auto_format_group_name)
        vim.notify(
            "Formatting on save disabled",
            vim.log.levels.INFO,
            { title = plugin_name }
        )
    end

    function M.toggle_auto_format()
        if vim.fn.exists("#" .. auto_format_group_name .. "#BufWritePre") == 0 then
            M.enable_auto_format()
        else
            M.disable_auto_format()
        end
    end

    vim.api.nvim_create_user_command(
        "LspToggleFormatOnSave",
        function()
            M.toggle_auto_format()
        end,
        {
            nargs = 0,
            desc = "Toggle formatting on save",
        }
    )

    vim.api.nvim_create_user_command(
        "LspEnableFormatOnSave",
        function()
            M.enable_auto_format()
        end,
        {
            nargs = 0,
            desc = "Enable formatting on save",
        }
    )

    vim.api.nvim_create_user_command(
        "LspDisableFormatOnSave",
        function()
            M.disable_auto_format()
        end,
        {
            nargs = 0,
            desc = "Disable formatting on save",
        }
    )
end

vim.api.nvim_create_autocmd("LspAttach", {
    desc = "Setup formatting on save",
    callback = function(event) setup_auto_format(event) end,
})

return M
