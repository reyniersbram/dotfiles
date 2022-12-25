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

-- local compare = require "cmp.config.compare"

require("luasnip/loaders/from_vscode").lazy_load()

local check_backspace = function()
	local col = vim.fn.col(".") - 1
	return col == 0 or vim.fn.getline("."):sub(col, col):match("%s")
end


local icons = require("config.icons")
local kind_icons = icons.kind

-- vim.g.cmp_active = true

cmp.setup({
--     enabled = function()
--         local buftype = vim.api.nvim_buf_get_option(0, "buftype")
--         if buftype == "prompt" then
--             return false
--         end
--         return vim.g.cmp_active
--     end,

--     preselect = cmp.PreselectMode.None,

    snippet = {
        expand = function(args)
            luasnip.lsp_expand(args.body) -- For `luasnip` users.
        end,
    },
	mapping = {
        ["<C-k>"] = cmp.mapping.select_prev_item(),
		["<C-j>"] = cmp.mapping.select_next_item(),
		["<C-b>"] = cmp.mapping(cmp.mapping.scroll_docs(-1), { "i", "c" }),
		["<C-f>"] = cmp.mapping(cmp.mapping.scroll_docs(1), { "i", "c" }),
		["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
		["<C-y>"] = cmp.config.disable, -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
		["<C-c>"] = cmp.mapping({
            i = cmp.mapping.abort(),
			c = cmp.mapping.close(),
		}),
		-- Accept currently selected item. If none selected, `select` first item.
		-- Set `select` to `false` to only confirm explicitly selected items.
		["<CR>"] = cmp.mapping.confirm({ select = true }),
		["<Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
				cmp.select_next_item()
			elseif luasnip.expandable() then
				luasnip.expand()
			elseif luasnip.expand_or_jumpable() then
				luasnip.expand_or_jump()
			elseif check_backspace() then
				fallback()
			else
				fallback()
			end
        end, {
			"i",
			"s",
		}),
		["<S-Tab>"] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_prev_item()
			elseif luasnip.jumpable(-1) then
				luasnip.jump(-1)
			else
				fallback()
			end
        end, {
			"i",
			"s",
		}),
    },
    formatting = {
        fields = { "kind", "abbr", "menu" },
        format = function(entry, vim_item)
            -- Kind icons
            vim_item.kind = string.format("%s %s", kind_icons[vim_item.kind], vim_item.kind) -- This concatonates the icons with the name of the item kind

--             if entry.source.name == "emoji" then
--                 vim_item.kind = icons.misc.Smiley
--                 vim_item.kind_hl_group = "CmpItemKindEmoji"
--             end
            -- NOTE: order matters
            vim_item.menu = ({
                luasnip = "[Snippet]",
                buffer = "[Buffer]",
                path = "[Path]",
                nvim_lsp = "[LSP]",
                nvim_lua = "[Lua]",
                spell = "[Spell]",
                calc = "[Calc]",
                emoji = "[Emoji]",
            })[entry.source.name]
            return vim_item
        end,
    },
    sources = {
--         { name = "nvim_lsp",
--         entry_filter = function(entry, ctx)
--             local kind = require("cmp.types.lsp").CompletionItemKind[entry:get_kind()]
--             --[[ if kind == "Snippet" and ctx.prev_context.filetype == "java" then ]]
--             ----[[   return false ]]
--             ----[[ end ]]
--             if kind == "Text" then
--                 return false
--             end
--             return true
--         end,
--         group_index = 2,
--     },
--         { name = "nvim_lua", group_index = 2 },
        { name = "luasnip", group_index = 2 },
--         { name = "calc" , group_index = 2 },
        { name = "buffer", group_index = 2,
--         entry_filter = function(entry, ctx)
--             if not contains(buffer_fts, ctx.prev_context.filetype) then
--                 return false
--             end
--             return true
--         end,
        },
        { name = "path", group_index = 2 },
--         { name = "emoji" , group_index = 2 },
--         { name = "spell", group_index = 2 },
    },
--     sorting = {
--         priority_weight = 2,
--         comparators = {
--             -- require("copilot_cmp.comparators").prioritize,
--             -- require("copilot_cmp.comparators").score,
--             compare.offset,
--             compare.exact,
--             -- compare.scopes,
--             compare.score,
--             compare.recently_used,
--             compare.locality,
--             -- compare.kind,
--             compare.sort_text,
--             compare.length,
--             compare.order,
--             -- require("copilot_cmp.comparators").prioritize,
--             -- require("copilot_cmp.comparators").score,
--         },
--     },
    confirm_opts = {
            behavior = cmp.ConfirmBehavior.Replace,
            select = false,
    },
    window = {
        documentation = cmp.config.window.bordered(),
    },
    experimental = {
        ghost_text = true,
        native_menu = false,
    },
})
