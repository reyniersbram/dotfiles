local M = {}

local function progress()
    local cur = vim.fn.line('.')
    local total = vim.fn.line('$')
    if cur == 1 then
        return 'Top'
    elseif cur == total then
        return 'Bot'
    else
        return string.format('%2d%%%%', math.floor(cur / total * 100))
    end
end

local function lsp_progress()
    return vim.lsp.status()
end

function M.render()
    return '%<%f %h%m%r%=' .. lsp_progress() .. '%=%-14.(%l,%v%V%) ' .. progress()
end


local create_autocmd = require("core.autocmd").create_autocmd
create_autocmd("LspProgress", {
    desc = "Redraw statusline on LSP progress updates",
    callback = function()
        -- WARN: Experimental API, could use these as alternative
        -- vim.cmd("redrawstatus!") -- both statusline and winbar
        -- vim.cmd("redrawtabline") -- only tabline
        vim.api.nvim__redraw({
            statusline = true,
            winbar = true,
            tabline = true,
        })
    end,
})

vim.opt.ruler = false
vim.opt.statusline = '%!v:lua.require("core.statusline").render()'
vim.opt.laststatus = 3

-- TODO:
vim.opt.showtabline = 1
vim.opt.tabline = ""
-- tabpagemax
vim.opt.statuscolumn = ""
vim.opt.winbar = ""

return M
