local kind_icons_fallback = {
    __index = function(_, _)
        ---@param ctx blink.cmp.DrawItemContext
        ---@return string, string
        local function geticon(ctx)
            return ctx.kind_icon, ctx.kind_hl
        end
        return geticon
    end
}

---@alias KindIconProvider fun(ctx: blink.cmp.DrawItemContext): string, string
---@type table<string, KindIconProvider>
local kind_icon_providers = {
    -- TODO: Cmdline...
    Path = function(ctx)
        if ctx.item.data.type == "directory" then
            return require("util.icons").files.folder.folder, ctx.kind_hl
        end
        local dev_icon, dev_hl = require("nvim-web-devicons").get_icon(ctx.label)
        if dev_icon then
            return dev_icon, dev_hl
        end
        return require("util.icons").files.file, ctx.kind_hl
    end,
}
kind_icon_providers = setmetatable(kind_icon_providers, kind_icons_fallback)

return {
    "saghen/blink.cmp",
    dependencies = {
        "rafamadriz/friendly-snippets",
        "nvim-tree/nvim-web-devicons",
    },
    event = { "InsertEnter", "CmdlineEnter" },
    version = "*",
    config = function()
        local blink = require("blink.cmp")
        blink.setup {
            keymap = { preset = "default" },
            appearance = {
                nerd_font_variant = "mono",
                kind_icons = require("util.icons").programming.completion_item_kind,
            },
            completion = {
                keyword = { range = "prefix" },
                accept = { auto_brackets = { enabled = true } },
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
                            { "label",      "label_description", gap = 1 },
                            { "source_name" },
                        },
                        components = {
                            kind_icon = {
                                text = function(ctx)
                                    local kind_icon, _ = kind_icon_providers
                                        [ctx.source_name](ctx)
                                    return kind_icon
                                end,
                                highlight = function(ctx)
                                    local _, kind_hl = kind_icon_providers
                                        [ctx.source_name](ctx)
                                    return kind_hl
                                end
                            },
                        },
                        treesitter = { "lsp" },
                    },
                },
                ghost_text = { enabled = true, },
            },
            sources = {
                default = { "lsp", "path", "snippets", "buffer" },
                providers = {
                    path = {
                        opts = {
                            get_cwd = function(_) return vim.fn.getcwd() end
                        },
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
                    preset = "cmdline",
                },
                completion = {
                    menu = {
                        auto_show = true,
                    },
                },
            },
        }
    end,
}
