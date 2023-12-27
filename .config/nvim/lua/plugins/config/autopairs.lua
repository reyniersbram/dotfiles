local status_ok, autopairs = pcall(require, "nvim-autopairs")
if not status_ok then
    vim.notify("autopairs not found!")
    return
end

autopairs.setup {
    disable_filetype = { "TelescopePrompt" },
    disable_in_macro = false,
    disable_in_visualblock = false,
    disable_in_replace_mode = true,
    ignored_next_char = [=[[%w%%%'%[%"%.%`%$]]=],
    enable_move_right = true,
    enable_afterquote = true,
    enable_check_bracket_in_quote = true,
    enable_check_bracket_line = false,
    enable_abbr = false,
    break_undo = true,
    check_ts = true,
    map_cr = true,
    map_bs = true,
    map_c_h = false,
    map_c_w = false,

    ts_config = {
        lua = { "string" },
        javascript = { "template_string" },
        java = false,
    },

    fast_wrap = {
        map = "<M-e>",
        chars = { "{", "[", "(", '"', "'", "<" },
        pattern = [=[[%'%"%>%]%)%)%}%,]]=],
        end_key = "$",
        keys = "qwertyuiopzxcvbnmasdfghjkl",
        check_comma = true,
        highlight = "Search",
        highlight_grey = "Comment",
    },
}

require("util").try_with_module("cmp", function(cmp)
    local cmp_autopairs = require("nvim-autopairs.completion.cmp")
    cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done())
end)
