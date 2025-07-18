local parser_dir = vim.fn.stdpath("data") .. "/treesitter"
vim.opt.runtimepath:append(parser_dir)

local default_parsers = {
    "c",
    "lua",
    "markdown", "markdown_inline",
    "query",
    "vim", "vimdoc",
}

local parsers = {
    "css",
    "editorconfig",
    "gitignore",
    "go", "gomod", "gosum",
    "haskell",
    "html",
    "javascript", "typescript",
    "json",
    "luadoc",
    "prolog",
    "python",
    "toml", "yaml",
    "vue",
}

---@class TextobjectKeymap
---@field query string
---@field desc string

---@param mode string|table<string> The mode
---@param keymaps table<string, TextobjectKeymap> The table of keymaps
---@param fn function(string, string?) The function to use for the mapping
local function set_keymaps(mode, keymaps, fn)
    local keymap = require("core.keymap")
    for key, map in pairs(keymaps) do
        keymap(
            mode,
            key,
            function()
                fn(map.query, "textobjects")
            end,
            { desc = map.desc }
        )
    end
end

return {
    {
        "nvim-treesitter/nvim-treesitter",
        branch = "main",
        build = ":TSUpdate",
        lazy = false,
        -- dependencies = {
        --     "nvim-treesitter/nvim-treesitter-textobjects",
        -- },
        config = function()
            local ts_config = require("nvim-treesitter.config")
            ts_config.setup {
                install_dir = parser_dir,
            }
            local ts_install = require("nvim-treesitter.install")
            ts_install.install(vim.list_extend(parsers, default_parsers))

            vim.api.nvim_create_autocmd("FileType", {
                callback = function(event)
                    local bufnr = event.buf
                    local filetype = event.match

                    if filetype == "" then
                        return
                    end

                    -- Get parser name from filetype
                    local parser_name = vim.treesitter.language.get_lang(
                        filetype)
                    if not parser_name then
                        vim.notify(
                            ("No treesitter parser found for %s"):format(
                                filetype),
                            vim.log.levels.DEBUG)
                        return
                    end

                    -- Start treesitter if the parser is installed
                    if vim.tbl_contains(ts_config.get_installed(), parser_name) then
                        vim.bo[bufnr].indentexpr =
                        "v:lua.require'nvim-treesitter'.indentexpr()"
                        vim.treesitter.start(bufnr, parser_name)
                        return
                    end

                    -- Notify the user a parser is available
                    if vim.tbl_contains(ts_config.get_available(), parser_name) then
                        vim.notify(
                            ("Parser available for %s, install the parser with `:TSInstall %s`.")
                            :format(filetype, parser_name),
                            vim.log.levels.INFO
                        )
                        return
                    else
                        vim.notify(
                            ("No treesitter parser found for %s"):format(
                                filetype),
                            vim.log.levels.DEBUG
                        )
                        return
                    end
                end
            })

            -- TODO: port these to the treesitter main branch
            --         textobjects = require("plugins.treesitter-modules.textobjects")
            --
            --     -- TODO:
            --     -- https://github.com/nvim-treesitter/nvim-treesitter?tab=readme-ov-file#folding
        end,
    },
    {
        "nvim-treesitter/nvim-treesitter-textobjects",
        branch = "main",
        config = function()
            local ts_textobjects = require("nvim-treesitter-textobjects")
            ts_textobjects.setup({
                select = {
                    lookahead = true,
                    selection_modes = {},
                    include_surrounding_whitespace = function(args)
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
                move = {
                    set_jumps = true,
                }
            })

            local ts_textobjects_select = require(
                "nvim-treesitter-textobjects.select")
            local ts_textobjects_swap = require(
                "nvim-treesitter-textobjects.swap")
            local ts_textobjects_move = require(
                "nvim-treesitter-textobjects.move")

            local select_keymaps = {
                ["a="] = { query = "@assignment.outer", desc = "Select around assignment" },
                ["i="] = { query = "@assignment.inner", desc = "Select inside assignment" },
                -- BUG: 'l' is a motion, so e.g. 'dl' will hang
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
            }
            local swap_next_keymaps = {
                ["<leader>sa"] = { query = "@parameter.inner", desc = "Swap with next parameter/argument" },
                ["<leader>sf"] = { query = "@function.outer", desc = "Swap with next function/method declaration" },
                ["<leader>s="] = { query = "@assignment.outer", desc = "Swap with next assignment" },
            }
            local swap_previous_keymaps = {
                ["<leader>sA"] = { query = "@parameter.inner", desc = "Swap with previous parameter/argument" },
                ["<leader>sFd"] = { query = "@function.outer", desc = "Swap with previous function/method declaration" },
                ["<leader>s+"] = { query = "@assignment.outer", desc = "Swap with previous assignment" },
            }
            local move_next_start = {
                ["]="] = { query = "@assignment.outer", desc = "Next assignment" },
                -- TODO: clashes with default ]a
                ["]a"] = { query = "@parameter.outer", desc = "Next parameter/argument" },
                ["]i"] = { query = "@conditional.outer", desc = "Next conditional" },
                -- TODO: clashes with default ]l
                ["]l"] = { query = "@loop.outer", desc = "Next loop" },
                ["]fc"] = { query = "@call.outer", desc = "Next function/method call" },
                ["]fd"] = { query = "@function.outer", desc = "Next function/method declaration" },
                ["]c"] = { query = "@class.outer", desc = "Next class" },
            }
            local move_previous_start = {
                ["[="] = { query = "@assignment.outer", desc = "Previous assignment" },
                ["[a"] = { query = "@parameter.outer", desc = "Previous parameter/argument" },
                ["[i"] = { query = "@conditional.outer", desc = "Previous conditional" },
                ["[l"] = { query = "@loop.outer", desc = "Previous loop" },
                ["[fc"] = { query = "@call.outer", desc = "Previous function/method call" },
                ["[fd"] = { query = "@function.outer", desc = "Previous function/method declaration" },
                ["[c"] = { query = "@class.outer", desc = "Previous class" },
            }
            local move_next_end = {
                ["]I"] = { query = "@conditional.outer", desc = "Next end of conditional" },
                ["]Fd"] = { query = "@function.outer", desc = "Next end of function/method declaration" },
                ["]c"] = { query = "@class.outer", desc = "Next end of class" },
            }
            local move_previous_end = {
                ["[I"] = { query = "@conditional.outer", desc = "Previous end of conditional" },
                ["[Fd"] = { query = "@function.outer", desc = "Previous end of function/method declaration" },
                ["[c"] = { query = "@class.outer", desc = "Previous end of class" },
            }
            set_keymaps({ "x", "o" }, select_keymaps,
                ts_textobjects_select.select_textobject)
            set_keymaps("n", swap_next_keymaps, ts_textobjects_swap.swap_next)
            set_keymaps("n", swap_previous_keymaps,
                ts_textobjects_swap.swap_previous)
            set_keymaps({ "n", "x", "o" }, move_next_start,
                ts_textobjects_move.goto_next_start)
            set_keymaps({ "n", "x", "o" }, move_previous_start,
                ts_textobjects_move.goto_previous_start)
            set_keymaps({ "n", "x", "o" }, move_next_end,
                ts_textobjects_move.goto_next_end)
            set_keymaps({ "n", "x", "o" }, move_previous_end,
                ts_textobjects_move.goto_previous_end)
        end
    }
}
