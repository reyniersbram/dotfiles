return {
    "folke/lazydev.nvim",
    ft = "lua",
    config = function()
        local lazydev = require("lazydev")
        lazydev.setup {
            debug = false,
            runtime = vim.env.VIMRUNTIME,
            library = {
                { path = "${3rd}/luv/library", words = { "vim%.uv" } },
            },
            integrations = {
                lspconfig = false,
                cmp = false,
                coq = false,
            },
            enabled = function (root_dir)
                -- enable or disable when `vim.g.lazydev_enabled` is explicitly set
                if vim.g.lazydev_enabled ~= nil then
                    return vim.g.lazydev_enabled
                end
                -- disable when .luarc.json is found
                return not vim.uv.fs_stat(root_dir .. "/.luarc.json")
            end
        }
    end
}
