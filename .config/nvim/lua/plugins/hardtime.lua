return {
    "m4xshen/hardtime.nvim",
    cond = false,
    dependencies = {
        "MunifTanjim/nui.nvim",
        "nvim-lua/plenary.nvim",
    },
    opts = {
        max_count = 3,
        disable_mouse = false,
        disabled_filetypes = {
            "",
            "alpha",
            "help",
            "lazy",
            "lspinfo",
            "mason",
            "NvimTree",
            "prompt",
            "TelescopePrompt",
        }
    },
}
