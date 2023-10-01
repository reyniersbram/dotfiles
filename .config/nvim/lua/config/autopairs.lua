local status_ok, npairs = pcall(require, "nvim-autopairs")
if not status_ok then
    vim.notify("autopairs not found!")
    return
end

npairs.setup {
    check_ts = true,
    ts_config = {
        lua = { "string", "source" },
        javascript = { "string", "template_string" },
        java = false,
    },
    disable_filetype = { "TelescopePrompt" },
    fast_wrap = {
        map = "<M-e>",
        chars = { "{", "[", "(", '"', "'" },
        pattern = string.gsub([[ [%'%"%)%>%]%)%}%,] ]], "%s+", ""),
        offset = 0, -- Offset from pattern match
        end_key = "$",
        keys = "qwertyuiopzxcvbnmasdfghjkl",
        check_comma = true,
        highlight = "PmenuSel",
        highlight_grey = "LineNr",
    },
}

-- local auto_cmp_status_ok, cmp_autopairs = pcall(require, "nvim-autopairs.completion.cmp")
-- if not auto_cmp_status_ok then
--     vim.notify "nvim-autopairs.completion.cmp not found!"
--     return
-- end
-- local cmp_status_ok, cmp = pcall(require, "cmp")
-- if not cmp_status_ok then
--     vim.notify("cmp not found!")
--     return
-- end
-- cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done { map_char = { tex = "" } })

