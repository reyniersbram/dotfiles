local M = {}

---@class treesitter.start.Config
---@field log_level vim.log.levels

---@type treesitter.start.Config
local default_config = {
    log_level = vim.log.levels.INFO,
}

---@param user_config? treesitter.start.Config
function M.setup(user_config)
    local config = vim.tbl_deep_extend("force", default_config, user_config or {})

    local ts_config = require("nvim-treesitter.config")
    vim.api.nvim_create_autocmd("FileType", {
        callback = function(event)
            local bufnr = event.buf
            local filetype = event.match

            if filetype == "" then
                return
            end

            -- Get parser name from filetype
            local parser_name = vim.treesitter.language.get_lang(
                filetype)
            if not parser_name then
                if config.log_level <= vim.log.levels.DEBUG then
                    vim.notify(
                        ("No treesitter parser found for %s"):format(
                            filetype),
                        vim.log.levels.DEBUG)
                    return
                end
            end

            -- Start treesitter if the parser is installed
            if vim.tbl_contains(ts_config.get_installed(), parser_name) then
                vim.bo[bufnr].indentexpr =
                "v:lua.require'nvim-treesitter'.indentexpr()"
                vim.treesitter.start(bufnr, parser_name)
                return
            end

            -- Notify the user a parser is available
            if vim.tbl_contains(ts_config.get_available(), parser_name) then
                if config.log_level <= vim.log.levels.INFO then
                    vim.notify(
                        ("Parser available for %s, install the parser with `:TSInstall %s`.")
                        :format(filetype, parser_name),
                        vim.log.levels.INFO
                    )
                    return
                end
            else
                if config.log_level <= vim.log.levels.DEBUG then
                    vim.notify(
                        ("No treesitter parser found for %s"):format(
                            filetype),
                        vim.log.levels.DEBUG
                    )
                    return
                end
            end
        end
    })
end

return M
