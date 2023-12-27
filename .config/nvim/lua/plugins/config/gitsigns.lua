local gitsigns = require("gitsigns")
local icons = require("util.icons")

local function on_attach(buffnr)
    local function opts(desc)
        return { noremap = true, silent = true, desc = desc }
    end
    vim.keymap.set("n", "<leader>gb", gitsigns.blame_line, opts("git blame"))
end

gitsigns.setup({
    signs = {
        add = {
            text = icons.git.add,
            hl = "gitsignsadd",
            numhl = "gitsignsaddnr",
            linehl = "gitsignsaddln",
        },
        change = {
            text = icons.git.mod,
            hl = "gitsignschange",
            numhl = "gitsignschangenr",
            linehl = "gitsignschangeln",
        },
        delete = {
            text = icons.git.remove,
            hl = "gitsignsdelete",
            numhl = "gitsignsdeletenr",
            linehl = "gitsignsdeleteln",
        },
        topdelete = {
            text = icons.git.remove,
            hl = "gitsignstopdelete",
            numhl = "gitsignsdeletenr",
            linehl = "gitsignsdeleteln",
        },
        changedelete = {
            text = icons.git.mod,
            hl = "gitsignschangedelete",
            numhl = "gitsignschangenr",
            linehl = "gitsignschangeln",
        },
        untracked = {
            test = "┆",
            hl = "gitsignsuntracked",
            numhl = "gitsignsuntrackednr",
            linehl = "gitsignsuntrackedln",
        },
    },
    signcolumn = true,          -- toggle with `:gitsigns toggle_signs`
    numhl = false,              -- toggle with `:gitsigns toggle_numhl`
    linehl = false,             -- toggle with `:gitsigns toggle_linehl`
    word_diff = false,          -- toggle with `:gitsigns toggle_word_diff`
    show_deleted = false,       -- toggle with `:gitsigns toggle_deleted`
    current_line_blame = false, -- toggle with `:gitsigns toggle_current_line_blame`
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
    current_line_blame_formatter = "<author>, <author_time:%y-%m-%d> - <summary>",
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
        -- options passed to nvim_open_win
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
