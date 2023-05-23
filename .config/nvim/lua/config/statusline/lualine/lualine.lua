local lualine_status_ok, lualine = pcall(require, "lualine")
if not lualine_status_ok then
    vim.notify("lualine not found")
    return
end
local navic_status_ok, navic = pcall(require, "nvim-navic")
if not navic_status_ok then
    vim.notify("navic not found")
    return
end

local components = require "config.statusline.lualine.components"

local extensions = {}
extensions.nvim_tree = {
    sections = {
        lualine_a = {
            function ()
                return vim.fn.fnamemodify(vim.fn.getcwd(), ':~')
            end
        },
        lualine_b = {
            components.branch
        },
    },
    filetypes = { 'NvimTree' }
}

lualine.setup({
    options = {
        icons_enabled = true,
        theme = 'auto',
        component_separators = {
            left = '',
            right = '',
        },
        section_separators = {
            left = '',
            right = '',
        },
        disabled_filetypes = {
            statusline = {},
            winbar = {},
        },
        ignore_focus = {},
        always_divide_middle = true,
        globalstatus = true,
        refresh = {
            statusline = 1000,
            tabline = 1000,
            winbar = 1000,
        },
    },
    sections = {
        lualine_a = { 'mode' },
        lualine_b = { components.diagnostics, components.whitespace, components.mixed_indent },
        lualine_c = { components.filename },
        lualine_x = { components.lsp_progress, components.fileformat, 'encoding', components.filetype },
        lualine_y = { components.diff, components.branch },
        lualine_z = {'progress', 'location'},
    },
    inactive_components = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = {},
        lualine_x = {},
        lualine_y = {},
        lualine_z = {},
    },
    tabline = {},
    winbar = {
        lualine_c = {
            {
                function ()
                    return navic.get_location()
                end,
                draw_empty = true,
            },
        },
    },
    inactive_winbar = {
        lualine_c = {
            {
                function ()
                    return navic.get_location()
                end,
                draw_empty = true,
            },
        },
    },
    extensions = { extensions.nvim_tree, 'toggleterm' }
})
