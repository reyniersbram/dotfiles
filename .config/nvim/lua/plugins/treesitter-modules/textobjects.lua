return {
    select = {
        enable = true,
        disable = {},
        lookahead = true,
        keymaps = {
            ["a="] = { query = "@assignment.outer", desc = "Select around assignment" },
            ["i="] = { query = "@assignment.inner", desc = "Select inside assignment" },
            -- BUG: 'l' is a motion, so e.g. 'dl' will wait
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
            ifd = { query = "@function.inner", desc = "Select inside function/method definition" },
            ac = { query = "@class.outer", desc = "Select around class declaration" },
            ic = { query = "@class.inner", desc = "Select inside class definition" },
            aq = { query = "@comment.outer", desc = "Select around comment" },
            iq = { query = "@comment.inner", desc = "Select inside comment" },
        },
        selection_modes = "v",
        include_surrounding_whitespace =
        ---@class WhiteSpaceArgs
        ---@field query_string string
        ---@field mode string

        ---@param args WhiteSpaceArgs
        ---@return boolean
            function(args)
                local triggers = {
                    "^@.*%.inner$",
                    "^@assignment.lhs$",
                    "^@assignment.rhs$",
                }
                for _, pattern in pairs(triggers) do
                    if string.find(args.query_string, pattern, 1, false) ~= nil then
                        return false
                    end
                end
                return true
            end,
    },
    swap = {
        enable = true,
        disable = {},
        swap_next = {
            ["<leader>sa"] = { query = "@parameter.inner", desc = "Swap with next parameter/argument" },
            ["<leader>sf"] = { query = "@function.outer", desc = "Swap with next function/method declaration" },
            ["<leader>s="] = { query = "@assignment.outer", desc = "Swap with next assignment" },
        },
        swap_previous = {
            ["<leader>sA"] = { query = "@parameter.inner", desc = "Swap with previous parameter/argument" },
            ["<leader>sFd"] = { query = "@function.outer", desc = "Swap with previous function/method declaration" },
            ["<leader>s+"] = { query = "@assignment.outer", desc = "Swap with previous assignment" },
        },
    },
    move = {
        enable = true,
        disable = {},
        set_jumps = true,
        goto_next_start = {
            ["]="] = { query = "@assignment.outer", desc = "Next assignment" },
            -- TODO: class with default ]a
            ["]a"] = { query = "@parameter.outer", desc = "Next parameter/argument" },
            ["]i"] = { query = "@conditional.outer", desc = "Next conditional" },
            -- TODO: class with default ]l
            ["]l"] = { query = "@loop.outer", desc = "Next loop" },
            ["]fc"] = { query = "@call.outer", desc = "Next function/method call" },
            ["]fd"] = { query = "@function.outer", desc = "Next function/method declaration" },
            ["]c"] = { query = "@class.outer", desc = "Next class" },
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
        goto_next_end = {
            ["]I"] = { query = "@conditional.outer", desc = "Next end of conditional" },
            ["]Fd"] = { query = "@function.outer", desc = "Next end of function/method declaration" },
            ["]c"] = { query = "@class.outer", desc = "Next end of class" },
        },
        goto_previous_end = {
            ["[I"] = { query = "@conditional.outer", desc = "Previous end of conditional" },
            ["[Fd"] = { query = "@function.outer", desc = "Previous end of function/method declaration" },
            ["[c"] = { query = "@class.outer", desc = "Previous end of class" },
        },
    },
    lsp_interop = {
        enable = true,
        floating_preview_opts = {},
        peek_definition_code = {
            ["<leader>pa"] = { query = "@assignment.outer", desc = "Peak assignment" },
            ["<leader>pf"] = { query = "@function.outer", desc = "Peak function/method definition" },
            ["<leader>pc"] = { query = "@class.outer", desc = "Peak class definition" },
        },
    },
}
