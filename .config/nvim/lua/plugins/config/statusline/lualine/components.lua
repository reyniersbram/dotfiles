local M = {}

local function get_color(group, attr)
    return vim.fn.synIDattr(vim.fn.synIDtrans(vim.fn.hlID(group)), attr)
end

local icons_status_ok, icons = pcall(require, "util.icons")
if not icons_status_ok then
    vim.notify("icons not found")
    return
end

M.branch = {
    "b:gitsigns_head",
    icon = icons.git.Branch,
    separator = "",
}

M.copilot = {
    "copilot",
    symbols = {
        status = {
            icons = require("util.icons").misc.copilot,
        },
        spinners = require("util.icons").spinners,
        -- spinner_color =
    },
    show_colors = true,
    show_loading = true,
    padding = { right = 2 },
}

M.diagnostics = {
    'diagnostics',
    sources = { 'nvim_lsp' },
    sections = { 'error', 'warn', 'info', 'hint' },
    symbols = {
        error = icons.diagnostics.ERROR .. " ",
        warn = icons.diagnostics.WARN .. " ",
        info = icons.diagnostics.INFO .. " ",
        hint = icons.diagnostics.HINT .. " ",
    },
    colored = true,
    update_in_insert = false,
    always_visible = true,
}

M.diff = {
    'diff',
    colored = true,
    symbols = {
        added = icons.git.Add .. " ",
        modified = icons.git.Mod .. " ",
        removed = icons.git.Remove .. " ",
    },
    source = function()
        local gitsigns = vim.b.gitsigns_status_dict
        if gitsigns then
            return {
                added = gitsigns.added,
                modified = gitsigns.changed,
                removed = gitsigns.removed,
            }
        end
    end,
    padding = { left = 0, right = 1 },
}

M.encoding = {
    "encoding",
}

M.fileformat = {
    'fileformat',
    symbols = {
        unix = icons.misc.OS.LinuxTux,
        dos = icons.misc.OS.Windows,
        mac = icons.misc.OS.Apple,
    },
    padding = { left = 0, right = 0 },
    separator = "",
}
M.filename = {
    'filename',
    file_status = true,
    newfile_status = false,
    path = 0,
    shorting_target = 40,
    symbols = {
        modified = icons.git.Mod,
        readonly = icons.documents.LockedFile,
        unnamed = "[No Name]", -- TODO: hide, e.g. for Alpha
        newfile = "[New]",
    }
}
M.filetype = {
    'filetype',
    colored = true,
    icon = { align = 'left' },
}

M.filetype_icon = {
    "filetype",
    colored = true,
    icon_only = true,
    separator = "",
    padding = { left = 1, right = 0 },
}

M.location = {
    "location",
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
        local space_indent_cnt = vim.fn.searchcount({ pattern = space_pattern, max_count = 1e3 }).total
        local tab_indent_cnt = vim.fn.searchcount({ pattern = tab_pattern, max_count = 1e3 }).total
        if space_indent_cnt > tab_indent_cnt then
            return tab_icon .. tab_indent
        else
            return tab_icon .. space_indent
        end
    end,
    color = { fg = get_color('DiagnosticInfo', "fg#") },
}

M.mode = {
    "mode",
}

M.navic = {
    function()
        return require("nvim-navic").get_location()
    end,
    cond = function()
        return package.loaded["nvim-navic"] and require("nvim-navic").is_available()
    end,
}

M.neovim = {
    function()
        return icons.misc.technology.Neovim
    end,
    draw_empty = true,
    separator = "",
}

M.progress = {
    "progress",
    separator = "",
    padding = { left = 1, right = 0 },
}

-- NOIMP:
function M.searchcount()
    local results = vim.fn.searchcount()
    return "[0/0]"
end

M.tabs = {
    "tabs",
    tab_max_lenght = 40,
    max_length = vim.o.columns / 3,
    mode = 0,
    path = 0,
    use_mode_colors = false,
    tabs_color = {
        active = "lualine_b_normal",
        inactive = "lualine_b_inactive",
    },
    show_modified_status = false,
    symbols = {
        modified = icons.git.Mod .. " ",
    },
}

M.whitespace = {
    function()
        local space = vim.fn.search([[\s\+$]], 'nwc')
        return space ~= 0 and icons.misc.keyboard.Space .. " " .. space or ""
    end,
    color = { fg = get_color('DiagnosticInfo', "fg#") },
}

return M
