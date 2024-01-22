local select_keymaps = {
    ["="] = { query = "assignment" },
    a = { query = "parameter", desc = "parameter/argument" },
    i = { query = "conditional" },
    l = { query = "loop" },
    fc = { query = "call", desc = "function/method call" },
    fd = { query = "function", desc = "function/method declaration" },
    c = { query = "class" },
}

local select_litteral_keymaps = {
    ["l="] = {
        query = "@assignment.lhs",
        desc = "Select left hand side of assignment",
    },
    ["r="] = {
        query = "@assignment.rhs",
        desc = "Select right hand side of assignment",
    },
}

local function convert_keymaps(keymaps, litteral_keymaps)
    local converted = {}
    for key, value in pairs(keymaps) do
        converted["a" .. key] = {
            query = "@" .. value.query .. ".outer",
            desc = "Select around " .. (value.desc or value.query),
        }
        converted["i" .. key] = {
            query = "@" .. value.query .. ".inner",
            desc = "Select inside " .. (value.desc or value.query),
        }
    end
    return vim.tbl_deep_extend("force", converted, litteral_keymaps)
end

require("util").try_with_module(
    "nvim-treesitter.configs",
    function(treesitter)
        treesitter.setup {
            textobjects = {
                select = {
                    enable = true,
                    lookahead = true,
                    include_surrounding_whitespace = true,
                    selection_modes = "v",
                    keymaps = convert_keymaps(select_keymaps, select_litteral_keymaps),
                },
                -- TODO:
                swap = {
                    enable = true,
                    swap_next = {

                    },
                    swap_previous = {

                    },
                },
                -- TODO:
                move = {
                    enable = true,
                    set_jumps = true,
                    goto_next_start = {
                    },
                    goto_next_end = {
                    },
                    goto_previous_start = {
                    },
                    goto_previous_end = {
                    },
                    goto_next = {
                    },
                    goto_previous = {
                    },
                },
                -- TODO:
                lsp_interop = {
                }
            },
        }
    end
)
require("util").try_with_module(
    "nvim-treesitter.textobjects.repeatable_move",
    function(ts_repeat_move)
        local modes = { "n", "x", "o" }
        vim.keymap.set(modes, ";", ts_repeat_move.repeat_last_move)
        vim.keymap.set(modes, ",", ts_repeat_move.repeat_last_move_opposite)
        vim.keymap.set(modes, "f", ts_repeat_move.builtin_f)
        vim.keymap.set(modes, "F", ts_repeat_move.builtin_F)
        vim.keymap.set(modes, "t", ts_repeat_move.builtin_t)
        vim.keymap.set(modes, "T", ts_repeat_move.builtin_T)
        require("util").try_with_module(
            "gitsigns",
            function(gitsigns)
                local next_hunk, prev_hunk =
                    ts_repeat_move.make_repeatable_move_pair(
                        gitsigns.next_hunk, gitsigns.prev_hunk
                    )
                -- TODO: figure this out, should still be in gitsigns config
                -- vim.keymap.set("n", "ghn", next_hunk, opts("Go to next git hunk"))
                -- vim.keymap.set("n", "ghp", prev_hunk, opts("Go to previous git hunk"))
                -- TODO: do the same for lsp diagnostics
            end
        )
    end
)
