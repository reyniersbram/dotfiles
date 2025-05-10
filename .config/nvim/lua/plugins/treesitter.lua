local parser_dir = vim.fn.stdpath("data") .. "/treesitter"
vim.opt.runtimepath:append(parser_dir)

---Notify the user when the parser for a file is not installed.
---@param ft string Filetype of the buffer
local function notify_missing_parser(ft)
    local ok, ts_parsers = pcall(require, "nvim-treesitter.parsers")
    if not ok then
        return
    end
    local all_parsers = ts_parsers.available_parsers()
    local has_parser = ts_parsers.has_parser(ft)
    if not has_parser and vim.tbl_contains(all_parsers, ft) then
        vim.notify(
            ("No tree-sitter parser installed for %s, install the parser with `:TSInstall %s`.")
            :format(ft, ft),
            vim.log.levels.INFO
        )
    end
end

require("core.autocmd").create_autocmd("FileType", {
    desc = "Notify missing tree-sitter parser",
    callback = function(ev)
        notify_missing_parser(ev.match)
    end,
})

local default_parsers = {
    "c",
    "lua",
    "markdown", "markdown_inline",
    "query",
    "vim", "vimdoc",
}

local parsers = {
    "editorconfig",
    "go", "gomod", "gosum",
    "json",
    "luadoc",
    "prolog",
    "python",
}

return {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    cmd = {
        "TSInstall", "TSInstallSync", "TSInstallInfo", "TSUninstall",
        "TSUpdate", "TSUpdateSync",
        "TSEnable", "TSDisable", "TSToggle",
        "TSModuleInfo", "TSEditQuery", "TSEditQueryUserAfter",
    },
    event = {
        "BufReadPost", "BufNewFile",
    },
    config = function()
        local treesitter = require("nvim-treesitter.configs")
        treesitter.setup {
            parser_install_dir = parser_dir,
            ensure_installed = vim.list_extend(parsers, default_parsers),
            ignore_install = {},
            sync_install = false,
            auto_install = false,
            modules = {},
            incremental_selection = {
                enable = false,
            }

        }
        treesitter.setup {
            highlight = {
                enable = true,
                disable = {},
                additional_vim_regex_highlighting = false,
            },
        }
        treesitter.setup {
            indent = {
                enable = true,
                disable = {},
            }
        }
    end,
}
