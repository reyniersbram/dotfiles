return {
    {
        "nvim-treesitter/nvim-treesitter",
        dependencies = {
            "JoosepAlviste/nvim-ts-context-commentstring",
        },
        build = ":TSUpdate",
        opts = {
            ensure_installed = "all",
            sync_install = false,
            ignore_install = { "" }, -- List of parsers to ignore installing
            autopairs = {
                enable = true,
            },
            highlight = {
                enable = true, -- false will disable the whole extension
                disable = { "" }, -- list of language that will be disabled
                additional_vim_regex_highlighting = true,
            },
            indent = {
                enable = true,
                disable = { "" }
            },
            context_commentstring = {
                enable = true,
                enable_autocmd = false,
            },
        }, 
    },
    {
        "JoosepAlviste/nvim-ts-context-commentstring",
        opts = {
            lua = {
                __default = "-- %s",
                __multiline = "--[[\n%s\n]]"
            },
        },
    },
}
