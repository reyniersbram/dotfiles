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
    -- vim.keymap.set("n", "<leader>gs", gitsigns.stage_buffer, opts("Git Stage current buffer"))
    -- vim.keymap.set("n", "<leader>gS", gitsigns.stage_hunk, opts("Git Stage current hunk"))
    -- vim.keymap.set("v", "<leader>gs", gitsigns.stage_hunk, opts("Git Stage current hunk"))
    -- vim.keymap.set("n", "<leader>gu", gitsigns.undo_stage_buffer, opts("Git Undo stage current buffer"))
    -- vim.keymap.set("n", "<leader>gU", gitsigns.undo_stage_hunk, opts("Git Undo stage current hunk"))
end

gitsigns.setup({
    signs = {
        add = {
            text = icons.git.Add,
            hl = "GitSignsAdd",
            numhl = "GitSignsAddNr",
            linehl = "GitSignsAddLn",
        },
        change = {
            text = icons.git.Mod,
            hl = "GitSignsChange",
            numhl = "GitSignsChangeNr",
            linehl = "GitSignsChangeLn",
        },
        delete = {
            text = icons.git.Remove,
            hl = "GitSignsDelete",
            numhl = "GitSignsDeleteNr",
            linehl = "GitSignsDeleteLn",
        },
        topdelete = {
            text = icons.git.Remove,
            hl = "GitSignsTopDelete",
            numhl = "GitSignsDeleteNr",
            linehl = "GitSignsDeleteLn",
        },
        changedelete = {
            text = icons.git.Mod,
            hl = "GitSignsChangedelete",
            numhl = "GitSignsChangeNr",
            linehl = "GitSignsChangeLn",
        },
        untracked = {
            test = "┆",
            hl = "GitSignsUntracked",
            numhl = "GitSignsUntrackedNr",
            linehl = "GitSignsUntrackedLn",
        },
    },
    signcolumn = true,          -- Toggle with `:Gitsigns toggle_signs`
    numhl = false,              -- Toggle with `:Gitsigns toggle_numhl`
    linehl = false,             -- Toggle with `:Gitsigns toggle_linehl`
    word_diff = false,          -- Toggle with `:Gitsigns toggle_word_diff`
    show_deleted = false,       -- Toggle with `:Gitsigns toggle_deleted`
    current_line_blame = false, -- Toggle with `:Gitsigns toggle_current_line_blame`
    diff_opts = {
        algorithm = "myers",    -- myers | minimal | patience | histogram
        -- internal = true,
        -- indent_heuristic = true,
        vertical = true,
        -- linematch = true,
        ignore_blank_lines = false,
        ignore_whitespace_change = false,
        ignore_whitespace = false,
        ignore_whitespace_change_at_eol = false,
    },
    current_line_blame_opts = {
        delay = 1000,
        virt_text = true,
        virt_text_pos = "eol", -- 'eol' | 'overlay' | 'right_align'
        virt_text_priority = 100,
        ignore_whitespace = false,
    },
    current_line_blame_formatter = "<author>, <author_time:%Y-%m-%d> - <summary>",
    current_line_blame_formatter_nc = "<author>",
    current_line_blame_formatter_opts = {
        relative_time = false,
    },
    base = "@",
    count_chars = {
        "1", "2", "3", "4", "5", "6", "7", "8", "9", ["+"] = ">",
    },
    watch_gitdir = {
        enable = true,
        follow_files = true,
    },
    attach_to_untracked = true,
    sign_priority = 6,
    update_debounce = 100,
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
    yadm = {
        enable = false,
    },
    trouble = false,
    on_attach = on_attach,
    debug_mode = false,
})
