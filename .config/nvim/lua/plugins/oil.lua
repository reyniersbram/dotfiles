local function toggle_float()
    local oil = require("oil")
    return oil.toggle_float()
end

return {
    "stevearc/oil.nvim",
    dependencies = {
        "nvim-tree/nvim-web-devicons",
    },
    cmd = { "Oil" },
    keys = {
        { "<leader>e", toggle_float, "n", desc = "Open file explorer" },
    },
    lazy = false,
    config = function()
        local oil = require("oil")
        oil.setup {
            default_file_explorer = true,
            columns = {
                "icon",
            },
            buf_options = {
                buflisted = false,
                bufhidden = "hide",
            },
            win_options = {
                wrap = false,
                signcolumn = "no",
                cursorcolumn = false,
                foldcolumn = "0",
                spell = false,
                list = false,
                conceallevel = 3,
                concealcursor = "nvic",
            },
            delete_to_trash = false,
            skip_confirm_for_simple_edits = false,
            prompt_save_on_select_new_entry = true,
            cleanup_delay_ms = 2000,
            lsp_file_method = {
                enabled = true,
                timeout_ms = 1000,
                autosave_changes = false,
            },
            constrain_cursor = "editable",
            watch_for_changes = true,
            keymaps = {
                ["g?"] = { "actions.show_help", mode = "n" },
                ["<CR>"] = "actions.select",
                ["<C-s>"] = { "actions.select", opts = { vertical = true } },
                ["<C-h>"] = { "actions.select", opts = { horizontal = true } },
                ["<C-t>"] = { "actions.select", opts = { tab = true } },
                ["<C-p>"] = "actions.preview",
                ["<C-c>"] = { "actions.close", mode = "n" },
                ["<C-l>"] = "actions.refresh",
                ["-"] = { "actions.parent", mode = "n" },
                ["_"] = { "actions.open_cwd", mode = "n" },
                ["`"] = { "actions.cd", mode = "n" },
                ["~"] = { "actions.cd", opts = { scope = "tab" }, mode = "n" },
                ["gs"] = { "actions.change_sort", mode = "n" },
                ["gx"] = "actions.open_external",
                ["g."] = { "actions.toggle_hidden", mode = "n" },
                ["g\\"] = { "actions.toggle_trash", mode = "n" },
            },
            use_default_keymaps = true,
            view_options = {
                show_hidden = true,
                is_hidden_file = function(name, bufnr)
                    local m = name:match("^%.")
                    return m ~= nil
                end,
                is_always_hidden = function(name, bufnr)
                    return false
                end,
                natural_order = "fast",
                case_insensitive = true,
                sort = {
                    { "name", "asc" },
                },
                highlight_filename = function(entry, is_hidden, is_link_target, is_link_orphan)
                    return nil
                end,
            },
            extra_scp_args = {},
            git = {
                add = function(path)
                    return false
                end,
                mv = function(src_path, dest_path)
                    return false
                end,
                rm = function(path)
                    return false
                end,
            },
            float = {
                padding = 2,
                max_width = 0.7,
                max_height = 0.7,
                border = require("util.icons").ui.border,
                win_options = {
                    winblend = math.min(5, vim.opt.winblend:get()),
                },
                get_win_title = nil,
                preview_split = "auto",
                override = function(conf)
                    return conf
                end,
            },
            preview_win = {
                update_on_cursor_moved = true,
                preview_method = "fast_scratch",
                disable_preview = function(filename)
                    return false
                end,
                win_options = {},
            },
            -- TODO:
            confirmation = {
                max_width = 0.9,
                min_width = { 40, 0.4 },
                width = nil,
                max_height = 0.9,
                min_height = { 5, 0.1 },
                height = nil,
                border = require("util.icons").ui.border,
                win_options = {
                    winblend = math.min(5, vim.opt.winblend:get()),
                },
            },
            -- TODO:
            progress = {
                max_width = 0.9,
                min_width = { 40, 0.4 },
                width = nil,
                max_height = { 10, 0.9 },
                min_height = { 5, 0.1 },
                height = nil,
                border = require("util.icons").ui.border,
                minimized_border = "none",
                win_options = {
                    winblend = math.min(5, vim.opt.winblend:get()),
                },
            },
            ssh = {
                border = require("util.icons").ui.border,
            },
            keymaps_help = {
                border = require("util.icons").ui.border,
            },
        }
    end,
}
