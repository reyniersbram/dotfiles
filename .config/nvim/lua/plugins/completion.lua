return {
    "saghen/blink.cmp",
    dependencies = {
        "rafamadriz/friendly-snippets",
        "nvim-tree/nvim-web-devicons",
        "folke/lazydev.nvim",
    },
    event = { "InsertEnter", "CmdlineEnter" },
    version = "*",
    config = function()
        local blink = require("blink.cmp")
        blink.setup {
            keymap = { preset = "default" },
            appearance = {
                nerd_font_variant = "mono",
            },
            completion = {
                keyword = { range = "prefix" },
                accept = { auto_brackets = { enabled = false } },
                list = {
                    selection = {
                        preselect = true,
                        auto_insert = false,
                    },
                },
                documentation = {
                    auto_show = true,
                    auto_show_delay_ms = 500,
                    update_delay_ms = 50,
                    treesitter_highlighting = true,
                    window = {
                        direction_priority = {
                            menu_north = { "e", "w", "n", "s" },
                            menu_south = { "e", "w", "s", "n" },
                        },
                    },
                },
                menu = {
                    auto_show = true,
                    border = "none",
                    direction_priority = { "s", "n" },
                    draw = {
                        columns = {
                            { "kind_icon" },
                            { "label", "label_description", gap = 1 },
                            { "source_name" },
                        },
                        components = {
                            kind_icon = {
                                text = function(ctx)
                                    if vim.tbl_contains({ "Path" }, ctx.source_name) then
                                        if ctx.item.data.type == "directory" then
                                            return require("util.icons").files.folder.folder
                                        end
                                        local dev_icon, _ = require("nvim-web-devicons").get_icon(ctx.label)
                                        return dev_icon or require("util.icons").files.file
                                    end
                                    -- TODO: LSP kinds, snippets, buffer...
                                    return ctx.kind_icon
                                end,
                                highlight = function(ctx)
                                    if vim.tbl_contains({ "Path" }, ctx.source_name) then
                                        local dev_icon, dev_hl = require("nvim-web-devicons").get_icon(ctx.label)
                                        if dev_icon then
                                            return dev_hl
                                        end
                                    end
                                    return { ctx.kind_hl }
                                end
                            },
                        },
                    },
                },
                ghost_text = { enabled = true, },
            },
            sources = {
                default = { "lazydev", "lsp", "path", "snippets", "buffer" },
                providers = {
                    lazydev = {
                        name = "LazyDev",
                        module = "lazydev.integrations.blink",
                        score_offset = 100,
                    },
                },
            },
            fuzzy = { implementation = "prefer_rust_with_warning" },
            signature = {
                enabled = true,
            },
            cmdline = {
                enabled = true,
                keymap = {
                    ['<Tab>'] = {
                        function(cmp)
                            if cmp.is_ghost_text_visible() and not cmp.is_menu_visible() then return cmp.accept() end
                        end,
                        'show_and_insert',
                        'select_next',
                    },
                    ['<S-Tab>'] = { 'show_and_insert', 'select_prev' },

                    ['<C-n>'] = { 'select_next', 'fallback' },
                    ['<C-p>'] = { 'select_prev', 'fallback' },

                    ['<C-y>'] = { 'select_and_accept' },
                    ['<C-e>'] = { 'cancel' },
                },
                completion = {
                    menu = { auto_show = true },
                },
            },
        }
    end,
}
