local function check_backspace()
    local col = vim.fn.col(".") - 1
    return col == 0 or vim.fn.getline("."):sub(col, col):match("%s")
end

local cmp_status_ok, cmp = pcall(require, "cmp")
if not cmp_status_ok then
    vim.notify("cmp not found!")
    return
end

local snip_status_ok, luasnip = pcall(require, "luasnip")
if not snip_status_ok then
    vim.notify("luasnip not found!")
    return
end

require("luasnip/loaders/from_vscode").lazy_load()

-- local buffer_fts = {
--     "markdown",
--     "toml",
--     "yaml",
--     "json",
-- }

-- local function contains(t, value)
--     for _, v in pairs(t) do
--        if v == value then
--             return true
--         end
--     end
--    return false
-- end

local icons = require("util.icons")

-- vim.g.cmp_active = true

cmp.setup {
    --[[ enabled = function()
        local buftype = vim.api.nvim_buf_get_option(0, "buftype")
        if buftype == "prompt" then
            return false
        end
        return vim.g.cmp_active
    end, ]]
    -- enabled = true,
    preselect = cmp.PreselectMode.None,
    snippet = {
        expand =
            function(args)
                luasnip.lsp_expand(args.body) -- set luasnip as snippet engine
            end,
    },
    completion = {
        keyword_length = 2,
        -- keyword_pattern = "",
        -- autocomplete = true,
        -- completeopt = "",
    },
    confimation = {
        -- get_commit_characters = nil,
    },
    formatting = {
        expandable_indicator = true,
        fields = { cmp.ItemField.Kind, cmp.ItemField.Abbr, cmp.ItemField.Menu },
        format =
            function(entry, vim_item)
                -- Kind icons
                vim_item.kind = string.format(
                    "%s %s",
                    icons.kind[vim_item.kind], vim_item.kind
                )

                --[[ if entry.source.name == "emoji" then
                    vim_item.kind = icons.misc.Smiley
                    vim_item.kind_hl_group = "CmpItemKindEmoji"
                end ]]
                vim_item.menu = ({
                    luasnip  = "[Snippet]",
                    buffer   = "[Buffer]",
                    path     = "[Path]",
                    nvim_lsp = "[LSP]",
                    nvim_lua = "[Nvim]",
                    -- spell       = "[Spell]",
                    -- calc        = "[Calc]",
                    emoji       = "[Emoji]",
                })[entry.source.name]
                return vim_item
            end,
    },
    matching = {
        disallow_fuzzy_matching = false,
        disallow_fullfuzzy_matching = false,
        disallow_partial_fuzzy_matching = false,
        disallow_partial_matching = false,
        disallow_prefix_matching = false,
    },
    sorting = {
        priority_weight = 2,
        comparators = {
            --[[ require("copilot_cmp.comparators").prioritize,
            require("copilot_cmp.comparators").score,
            cmp.compare.offset,
            cmp.compare.exact,
            cmp.compare.score,
            cmp.compare.recently_used,
            cmp.compare.kind,
            cmp.compare.sort_text,
            cmp.compare.length,
            cmp.compare.order,
            cmp.compare.locality,
            cmp.compare.scopes, ]]
        },
    },
    sources = {
        --[[ {
            name = "name",
            option = {}, -- source specific
            keyword_length = 3,
            keyword_pattern = "pattern",
            trigger_characters = {},
            priority = 0,
            max_item_count = 5,
            group_index = 2,
            entry_filter = function (entry, ctx) return true end,
        }, ]]
        {
            name = "nvim_lsp",
            group_index = 1,
            --[[ entry_filter = function(entry, ctx)
                local kind = require("cmp.types.lsp").CompletionItemKind[entry:get_kind()]
                if kind == "Snippet" and ctx.prev_context.filetype == "java" then
                    return false
                end
                if kind == "Text" then
                    return false
                end
                return true
            end, ]]
        },
        { name = "nvim_lua", group_index = 2 },
        { name = "luasnip", },
        {
            name = "buffer",
            group_index = 3,
            --[[ entry_filter = function(entry, ctx)
                if not contains(buffer_fts, ctx.prev_context.filetype) then
                    return false
                end
                return true
            end, ]]
        },
        { name = "path", },
        -- { name = "calc",  group_index = 2 },
        -- { name = "emoji", group_index = 2 },
        -- { name = "spell", group_index = 2 },
    },
    view = {
        entries = {
            name = "custom",
            -- selection_order = "near_cursor",
        },
    },

    window = {
        completion    = cmp.config.window.bordered(),
        documentation = cmp.config.window.bordered(),
    },

    mapping = {
        ["<C-k>"]     = cmp.mapping.select_prev_item {
            behavior = cmp.SelectBehavior.Select,
            count = 1,
        },
        ["<C-j>"]     = cmp.mapping.select_next_item {
            behavior = cmp.SelectBehavior.Select,
            count = 1,
        },
        ["<CR>"]      = cmp.mapping({
            -- only confirm when an item is selected
            i = function(fallback)
                if cmp.visible() and cmp.get_active_entry() then
                    cmp.confirm {
                        behavior = cmp.ConfirmBehavior.Replace,
                        select = false,
                    }
                else
                    fallback()
                end
            end,
            s = cmp.mapping.confirm { select = false },
            c = cmp.mapping.confirm {
                behavior = cmp.ConfirmBehavior.Replace,
                select = true,
            }
        }),
        -- ["<C-b>"]     = cmp.mapping(cmp.mapping.scroll_docs(1), { "i", "c" }),
        -- ["<C-f>"]     = cmp.mapping(cmp.mapping.scroll_docs(-1), { "i", "c" }),
        -- ["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
        ["<C-b>"]     = cmp.mapping.scroll_docs(1),
        ["<C-f>"]     = cmp.mapping.scroll_docs(-1),
        ["<C-Space>"] = cmp.mapping.complete {
            reason = cmp.ContextReason.Manual
        },
        -- ["<C-y>"] = cmp.config.disable, -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
        ["<C-c>"]     = cmp.mapping({
            i = cmp.mapping.abort(),
            c = cmp.mapping.close(),
        }),
        ["<Tab>"]     = cmp.mapping(
            function(fallback)
                if cmp.visible() then
                    cmp.select_next_item()
                elseif luasnip.expandable() then
                    luasnip.expand()
                elseif luasnip.expand_or_jumpable() then
                    luasnip.expand_or_jump()
                -- TODO understand why this is here
                elseif check_backspace() then
                    fallback()
                else
                    fallback()
                end
            end,
            { "i", "s" }
        ),
        ["<S-Tab>"]   = cmp.mapping(
            function(fallback)
                if cmp.visible() then
                    cmp.select_prev_item()
                elseif luasnip.jumpable(-1) then
                    luasnip.jump(-1)
                else
                    fallback()
                end
            end,
            { "i", "s" }
        ),
    },
    confirm_opts = {
        behavior = cmp.ConfirmBehavior.Replace,
    },
    experimental = {
        ghost_text = true,
    },
}

-- TODO figure out how I want this
-- cmp.setup.cmdline("/", {
--     mapping = cmp.mapping.preset.cmdline(),
--     sources = {
--         { name = "buffer", },
--     },
-- })
--
-- cmp.setup.cmdline(":", {
--     mapping = cmp.mapping.preset.cmdline(),
--     sources = cmp.config.sources({
--         { name = "cmdline", },
--         { name = "path", },
--         { name = "buffer", },
--     }),
-- })
