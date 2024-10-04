local gitsigns = require("gitsigns")
local icons = require("util.icons")

local function on_attach(buffnr)
    local function opts(desc)
        return { noremap = true, silent = true, desc = desc }
    end
    vim.keymap.set("n",
        "<leader>gb",
        gitsigns.blame_line,
        opts("Git Blame current line")
    )
    vim.keymap.set("n", "<leader>gd", gitsigns.diffthis, opts("Git Diff"))
    vim.keymap.set(
        "n",
        "<leader>gD",
        function()
            gitsigns.diffthis("@^")
        end,
        opts("Git Diff with parent")
    )
    vim.keymap.set("n", "ghn", gitsigns.next_hunk, opts("Go to next git hunk"))
    vim.keymap.set("n", "ghp", gitsigns.prev_hunk, opts("Go to previous git hunk"))
    vim.keymap.set("n", "<leader>gh", gitsigns.select_hunk, opts("Git select Hunk"))
    -- TODO: figure out if I want this
    -- vim.keymap.set("n", "<leader>gS", gitsigns.stage_buffer, opts("Git Stage current buffer"))
    vim.keymap.set("n", "<leader>gs", gitsigns.stage_hunk, opts("Git Stage current hunk"))
    vim.keymap.set("v", "<leader>gs", gitsigns.stage_hunk, opts("Git Stage current hunk"))
    vim.keymap.set("n", "<leader>gu", gitsigns.undo_stage_hunk, opts("Git Undo stage current hunk"))
end

gitsigns.setup({
    signs = {
        add = {
            text = icons.git.Add,
        },
        change = {
            text = icons.git.Mod,
        },
        delete = {
            text = icons.git.Remove,
        },
        topdelete = {
            text = icons.git.Remove,
        },
        changedelete = {
            text = icons.git.Mod,
        },
        untracked = {
            text = icons.git.Untracked,
        },
    },
    signs_staged = {
        add = {
            text = icons.git.Add,
        },
        change = {
            text = icons.git.Mod,
        },
        delete = {
            text = icons.git.Remove,
        },
        topdelete = {
            text = icons.git.Remove,
        },
        changedelete = {
            text = icons.git.Mod,
        },
        untracked = {
            text = icons.git.Untracked,
        },
    },
    signs_staged_enable = true,
    on_attach = on_attach,
    watch_gitdir = {
        enable = true,
        follow_files = true,
    },
    sign_priority = 6,
    signcolumn = true,       -- Toggle with `:Gitsigns toggle_signs`
    numhl = false,           -- Toggle with `:Gitsigns toggle_numhl`
    linehl = false,          -- Toggle with `:Gitsigns toggle_linehl`
    culhl = false,           -- Toggle with `:Gitsigns toggle_culhl`
    show_deleted = false,    -- Toggle with `:Gitsigns toggle_deleted`
    diff_opts = {
        algorithm = "myers", -- myers | minimal | patience | histogram
        -- internal = true,
        -- indent_heuristic = true,
        vertical = true,
        -- linematch = true,
        ignore_blank_lines = false,
        ignore_whitespace_change = false,
        ignore_whitespace = false,
        ignore_whitespace_change_at_eol = false,
    },
    base = "@",
    count_chars = {
        "1", "2", "3", "4", "5", "6", "7", "8", "9", ["+"] = ">",
    },
    status_formatter = nil,
    max_file_length = 40000,
    preview_config = {
        -- Options passed to nvim_open_win
        border = icons.ui.window.float.border,
        style = "minimal",
        relative = "cursor",
        row = 0,
        col = 1,
    },
    auto_attach = true,
    attach_to_untracked = true,
    update_debounce = 100,
    current_line_blame = false, -- Toggle with `:Gitsigns toggle_current_line_blame`
    current_line_blame_opts = {
        delay = 1000,
        virt_text = true,
        virt_text_pos = "eol", -- 'eol' | 'overlay' | 'right_align'
        virt_text_priority = 100,
        ignore_whitespace = false,
        use_focus = false,
    },
    current_line_blame_formatter = "<author>, <author_time:%Y-%m-%d> - <summary>",
    current_line_blame_formatter_nc = "<author>",
    trouble = false,
    word_diff = false, -- Toggle with `:Gitsigns toggle_word_diff`
    debug_mode = false,
})
