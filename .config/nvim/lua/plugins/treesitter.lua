local parser_dir = vim.fn.stdpath("data") .. "/treesitter"
vim.opt.runtimepath:append(parser_dir)

local default_parsers = {
    "c",
    "lua",
    "markdown", "markdown_inline",
    "query",
    "vim", "vimdoc",
}

local parsers = {
    "css",
    "editorconfig",
    "gitignore",
    "go", "gomod", "gosum",
    "haskell",
    "html",
    "javascript", "typescript",
    "json",
    "luadoc",
    "prolog",
    "python",
    "toml",
    "vue",
}

return {
    "nvim-treesitter/nvim-treesitter",
    branch = "main",
    build = ":TSUpdate",
    lazy = false,
    -- dependencies = {
    --     "nvim-treesitter/nvim-treesitter-textobjects",
    -- },
    config = function()
        local ts_config = require("nvim-treesitter.config")
        ts_config.setup {
            install_dir = parser_dir,
        }
        local ts_install = require("nvim-treesitter.install")
        ts_install.install(vim.list_extend(parsers, default_parsers))

        vim.api.nvim_create_autocmd("FileType", {
            callback = function(event)
                local bufnr = event.buf
                local filetype = event.match

                if filetype == "" then
                    return
                end

                -- Get parser name from filetype
                local parser_name = vim.treesitter.language.get_lang(filetype)
                if not parser_name then
                    vim.notify(
                        ("No treesitter parser found for %s"):format(filetype),
                        vim.log.levels.DEBUG)
                    return
                end

                -- Start treesitter if the parser is installed
                if vim.tbl_contains(ts_config.get_installed(), parser_name) then
                    vim.bo[bufnr].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
                    vim.treesitter.start(bufnr, parser_name)
                    return
                end

                -- Notify the user a parser is available
                if vim.tbl_contains(ts_config.get_available(), parser_name) then
                    vim.notify(
                        ("Parser available for %s, install the parser with `:TSInstall %s`.")
                        :format(filetype, parser_name),
                        vim.log.levels.INFO
                    )
                    return
                else
                    vim.notify(
                        ("No treesitter parser found for %s"):format(filetype),
                        vim.log.levels.DEBUG)
                    return
                end
            end
        })

        -- TODO: port these to the treesitter main branch
        --         incremental_selection = {
        --             enable = false,
        --         },
        --         textobjects = require("plugins.treesitter-modules.textobjects")
        --     }
        --
        --     -- TODO:
        --     -- https://github.com/nvim-treesitter/nvim-treesitter?tab=readme-ov-file#folding
    end,
}
