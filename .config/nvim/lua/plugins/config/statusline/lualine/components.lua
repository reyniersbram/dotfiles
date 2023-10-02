local M = {}

local icons_status_ok, icons = pcall(require, "helpers.icons")
if not icons_status_ok then
    vim.notify("icons not found")
    return
end

M.branch = {
    'branch',
    icon = icons.git.Branch,
}
M.diff = {
    'diff',
    icon = icons.git.Diff,
    symbols = {
        added = icons.git.Add .. " ",
        modified = icons.git.Mod .. " ",
        removed = icons.git.Remove .. " ",
    },
    colored = true,
}
M.diagnostics = {
    'diagnostics',
    sources = { 'nvim_lsp' },
    M = { 'error', 'warn', 'info', 'hint'},
    symbols = {
        error = icons.diagnostics.Error .. " ",
        warn = icons.diagnostics.Warning .. " ",
        info = icons.diagnostics.Information .. " ",
        hint = icons.diagnostics.Hint .. " ",
    },
    colored = true,
    update_in_insert = false,
    always_visible = true,
}
M.fileformat = {
    'fileformat',
    symbols = {
        unix = icons.misc.OS.LinuxTux,
        dos = icons.misc.OS.Windows,
        mac = icons.misc.OS.Apple,
    },
    separator = { right = '' },
    padding = { right = 0, left = 1 },
}
M.filename = {
    'filename',
    path = 4,
    -- TODO
}
M.filetype = {
    'filetype',
    colored = true,
    icon = { align = 'left', },
}
M.lsp_progress = {
    'lsp_progress',
    display_M = { { 'message' }, 'lsp_client_name' },
}

local function get_color(group, attr)
    return vim.fn.synIDattr(vim.fn.synIDtrans(vim.fn.hlID(group)), attr)
end
M.whitespace = {
    function ()
        local space = vim.fn.search([[\s\+$]], 'nwc')
        return space ~= 0 and icons.misc.keyboard.Space .. " " .. space or ""
    end,
    color = { fg = get_color('DiagnosticInfo', "fg#") },
}
M.mixed_indent = {
    function()
        local space_pattern = [[\v^ +]]
        local tab_pattern = [[\v^\t+]]
        local space_indent = vim.fn.search(space_pattern, 'nwc')
        local tab_indent = vim.fn.search(tab_pattern, 'nwc')
        local mixed = (space_indent > 0 and tab_indent > 0)
        local mixed_same_line
        local tab_icon = icons.misc.keyboard.Tab .. " "
        if not mixed then
            mixed_same_line = vim.fn.search([[\v^(\t+ | +\t)]], 'nwc')
            mixed = mixed_same_line > 0
        end
        if not mixed then return '' end
        if mixed_same_line ~= nil and mixed_same_line > 0 then
            return tab_icon .. mixed_same_line
        end
        local space_indent_cnt = vim.fn.searchcount({pattern=space_pattern, max_count=1e3}).total
        local tab_indent_cnt =  vim.fn.searchcount({pattern=tab_pattern, max_count=1e3}).total
        if space_indent_cnt > tab_indent_cnt then
            return tab_icon .. tab_indent
        else
            return tab_icon .. space_indent
        end
    end,
    color = { fg = get_color('DiagnosticInfo', "fg#") },
}

return M
