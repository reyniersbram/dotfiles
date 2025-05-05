local function find_files()
    local builtin = require("telescope.builtin")
    return builtin.find_files()
end

local function buffers()
    local builtin = require("telescope.builtin")
    return builtin.buffers()
end

local function live_grep()
    local builtin = require("telescope.builtin")
    return builtin.live_grep()
end

return {
    {
        'nvim-telescope/telescope.nvim',
        tag = '0.1.8',
        dependencies = {
            'nvim-lua/plenary.nvim',
            "nvim-tree/nvim-web-devicons",
            { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' }
        },
        cmd = { "Telescope" },
        keys = {
            {"<leader>ff", find_files, "n", desc = "Find file"},
            {"<leader>fb", buffers, "n", desc = "Find open buffer"},
            {"<leader>fg", live_grep, "n", desc = "Find text in files"},
        },
        config = function()
            local telescope = require("telescope")
            telescope.setup {
                defaults = {
                    cycle_layout_list = {
                        "horizontal", "vertical", "center", "cursor", "flex", "bottom_pane",
                    },
                    winblend = math.min(5, vim.opt.winblend:get()),
                    wrap_results = false,
                    prompt_prefix = require("util.icons").misc.telescope .. " ",
                    selection_caret = require("util.icons").ui.select .. " ",
                    entry_prefix = "  ",
                    multi_icon = '+', -- TODO:
                    path_display = {
                        shorten = { len = 2, exclude = { 1, -1 } },
                    },
                    dynamic_preview_title = true,
                    mappings = {},
                    file_ignore_patterns = { -- respects gitignore
                        "%~", -- vim backup files
                        ".git/", -- git directory
                    },
                },
                pickers = {
                    find_files = {
                        hidden = true,
                        no_ignore = false,
                        no_ignore_parents = false,
                    },
                },
                extensions = {
                    fzf = {},
                },
            }
            telescope.load_extension("fzf")
        end,
    },
}

