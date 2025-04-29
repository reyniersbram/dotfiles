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
                    keymaps = {
                        ["a="] = { query = "@assignment.outer", desc = "Select around assignment" },
                        ["i="] = { query = "@assignment.inner", desc = "Select inside assignment" },
                        ["l="] = { query = "@assignment.lhs", desc = "Select left hand side of assignment" },
                        ["r="] = { query = "@assignment.rhs", desc = "Select right hand side of assignment" },
                        aa = { query = "@parameter.outer", desc = "Select around parameter/argument" },
                        ia = { query = "@parameter.inner", desc = "Select inside parameter/argument" },
                        ai = { query = "@conditional.outer", desc = "Select around conditional" },
                        ii = { query = "@conditional.inner", desc = "Select inside conditional" },
                        al = { query = "@loop.outer", desc = "Select around loop" },
                        il = { query = "@loop.inner", desc = "Select inside loop" },
                        afc = { query = "@call.outer", desc = "Select around function/method call" },
                        ifc = { query = "@call.inner", desc = "Select inside function/method call" },
                        afd = { query = "@function.outer", desc = "Select around function/method declaration" },
                        ifd = { query = "@function.inner", desc = "Select inside function/method declaration" },
                        ac = { query = "@class.outer", desc = "Select around class" },
                        ic = { query = "@class.inner", desc = "Select inside class" },
                    },
                },
                swap = {
                    enable = true,
                    swap_next = {
                        ["<leader>sa"] = { query = "@parameter.inner", desc = "Swap with next parameter/argument" },
                        ["<leader>sfd"] = { query = "@function.outer", desc = "Swap with next function/method declaration" },
                        ["<leader>sfc"] = { query = "@call.outer", desc = "Swap with next function/method call" },
                    },
                    swap_previous = {
                        ["<leader>sA"] = { query = "@parameter.inner", desc = "Swap with previous parameter/argument" },
                        ["<leader>sFd"] = { query = "@function.outer", desc = "Swap with previous function/method declaration" },
                        ["<leader>sFc"] = { query = "@call.outer", desc = "Swap with previous function/method call" },
                    },
                },
                move = {
                    enable = true,
                    set_jumps = true,
                    goto_next_start = {
                        ["]="] = { query = "@assignment.outer", desc = "Next assignment" },
                        ["]a"] = { query = "@parameter.outer", desc = "Next parameter/argument" },
                        ["]i"] = { query = "@conditional.outer", desc = "Next conditional" },
                        ["]l"] = { query = "@loop.outer", desc = "Next loop" },
                        ["]fc"] = { query = "@call.outer", desc = "Next function/method call" },
                        ["]fd"] = { query = "@function.outer", desc = "Next function/method declaration" },
                        ["]c"] = { query = "@class.outer", desc = "Next class" },
                    },
                    goto_next_end = {
                        ["]I"] = { query = "@conditional.outer", desc = "Next end of conditional" },
                        ["]Fd"] = { query = "@function.outer", desc = "Next end of function/method declaration" },
                        ["]c"] = { query = "@class.outer", desc = "Next end of class" },
                    },
                    goto_previous_start = {
                        ["[="] = { query = "@assignment.outer", desc = "Previous assignment" },
                        ["[a"] = { query = "@parameter.outer", desc = "Previous parameter/argument" },
                        ["[i"] = { query = "@conditional.outer", desc = "Previous conditional" },
                        ["[l"] = { query = "@loop.outer", desc = "Previous loop" },
                        ["[fc"] = { query = "@call.outer", desc = "Previous function/method call" },
                        ["[fd"] = { query = "@function.outer", desc = "Previous function/method declaration" },
                        ["[c"] = { query = "@class.outer", desc = "Previous class" },
                    },
                    goto_previous_end = {
                        ["[I"] = { query = "@conditional.outer", desc = "Previous end of conditional" },
                        ["[Fd"] = { query = "@function.outer", desc = "Previous end of function/method declaration" },
                        ["[c"] = { query = "@class.outer", desc = "Previous end of class" },
                    },
                    goto_next = {},
                    goto_previous = {},
                },
                lsp_interop = {
                    enable = true,
                    floating_preview_opts = {
                        border = require("util.icons").ui.window.float.border,
                    },
                    peek_definition_code = {
                        ["<leader>pf"] = { query = "@function.outer", desc = "Peak function/method definition" },
                        ["<leader>pc"] = { query = "@class.outer", desc = "Peak class definition" },
                    },
                },
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
        vim.keymap.set(modes, "f", ts_repeat_move.builtin_f_expr, { expr = true })
        vim.keymap.set(modes, "F", ts_repeat_move.builtin_F_expr, { expr = true })
        vim.keymap.set(modes, "t", ts_repeat_move.builtin_t_expr, { expr = true })
        vim.keymap.set(modes, "T", ts_repeat_move.builtin_T_expr, { expr = true })
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
