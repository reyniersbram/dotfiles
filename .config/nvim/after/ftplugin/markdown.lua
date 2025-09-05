vim.opt_local.spell = true

vim.opt_local.makeprg = "pandoc % -o %<.pdf --citeproc"

---Run make asynchronous
---@param bufnr integer
local function async_make(bufnr)
    local filename = vim.api.nvim_buf_get_name(bufnr)
    local cmd = vim.fn.expandcmd(vim.opt.makeprg:get())
    vim.system(vim.fn.split(cmd), { text = true }, function(out)
        if out.code == 0 then
            vim.schedule(function()
                vim.notify("Compiled " .. filename)
            end)
        else
            vim.schedule(function()
                vim.notify(
                    "Compilation failed:\n" .. (out.stderr or "unknown error"),
                    vim.log.levels.ERROR)
            end)
        end
    end)
end

vim.api.nvim_buf_create_user_command(
    0,
    "MDAutocompile",
    function(args)
        ---@type boolean
        local enable
        if args.args == "on" then
            enable = true
        elseif args.args == "off" then
            enable = false
        else
            vim.notify(("Invalid argument: %s"):format(args.args),
                vim.log.levels.ERROR, {})
            return
        end

        local bufnr = vim.api.nvim_get_current_buf()
        local group_name = "MDAutocompile_" .. bufnr

        if enable then
            local group = vim.api.nvim_create_augroup(group_name,
                { clear = true })
            vim.api.nvim_create_autocmd("BufWritePost", {
                group = group,
                buffer = bufnr,
                desc = "Compile markdown file to pdf",
                callback = function()
                    async_make(bufnr)
                end
            })
        else
            pcall(vim.api.nvim_del_augroup_by_name, group_name)
        end
    end,
    {
        desc = "Manage automatic compilation of markdown files",
        complete = function()
            return { "on", "off" }
        end,
        nargs = 1,
    }
)
