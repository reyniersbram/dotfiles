local lualine_status_ok, lualine = pcall(require, "lualine")
if not lualine_status_ok then
    vim.notify("lualine not found")
    return
end

local components = require "plugins.config.statusline.lualine.components"

local extensions = {}
extensions.nvim_tree = {
    sections = {
        lualine_a = {
            function()
                return vim.fn.fnamemodify(vim.fn.getcwd(), ':~')
            end
        },
        lualine_b = {
            components.branch
        },
    },
    winbar = {},
    inactive_winbar = {},
    filetypes = { 'NvimTree' }
}
extensions.lazy = "lazy"
extensions.mason = "mason"
extensions.nvim_dap_ui = "nvim-dap-ui"

lualine.setup {
    options = {
        icons_enabled = true,
        theme = "auto",
        component_separators = require("util.icons").ui.separator.secondary,
        section_separators = require("util.icons").ui.separator.primary,
        disabled_filetypes = {
            statusline = { "alpha" },
            winbar = { "alpha" },
        },
        ignore_focus = {},
        always_divide_middle = false,
        globalstatus = true,
        refresh = {
            statusline = 1000,
            tabline = 1000,
            winbar = 1000,
        },
    },
    sections = {
        lualine_a = { components.neovim, components.mode },
        lualine_b = { components.diagnostics },
        lualine_c = {},
        lualine_x = { components.fileformat, components.encoding, components.filetype },
        lualine_y = { components.branch, components.diff },
        lualine_z = { components.progress, components.location },
    },
    inactive_sections = {},
    tabline = {
        -- TODO: figure out if I want this
        -- lualine_x = { components.tabs },
    },
    winbar = {
        lualine_b = { components.filetype_icon, components.filename },
        lualine_c = { components.navic },
    },
    inactive_winbar = {
        lualine_b = { components.filetype_icon, components.filename },
    },
    extensions = extensions,
}
