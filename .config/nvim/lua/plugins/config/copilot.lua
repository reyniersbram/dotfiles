local copilot = require("copilot")

copilot.setup {
    panel = {
        enable = true,
        auto_refresh = true,
        keymap = {
            jump_prev = "[[",
            jump_next = "]]",
            accept = "<CR>",
            refresh = "gr",
            open = "<M-CR>",
        },
        layout = {
            position = "bottom",
            ration = 0.4,
        },
    },
    suggestion = {
        enable = true,
        auto_trigger = true,
        debounce = 75,
        keymap = {
            accept = "<C-CR>",
            accept_word = false,
            accept_line = false,
            next = "<M-]>",
            prev = "<M-[>",
            dismiss = "<C-]",
        },
    },
    filetypes = {
        sh = function()
            local is_env = string.match(
                vim.fs.basename(vim.api.nvim_buf_get_name(0)),
                "^%.env.*$"
            )
            if is_env then
                return false
            end
            return true
        end,
    },
    copilot_node_comman = "node",
    server_opts_ovverrides = {},
}
