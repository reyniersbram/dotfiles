local ts_status_ok, treesitter = pcall(require, "nvim-treesitter.configs")
if not ts_status_ok then
    vim.notify("treesitter not found")
    return
end

local parser_dir = vim.fn.stdpath("data") .. "/treesitter"
vim.opt.rtp:append(parser_dir)

treesitter.setup {
    parser_install_dir = parser_dir,
    ensure_installed = {
        "arduino",
        "bash",
        "c", "cmake", "cpp",
        "css", "scss",
        "html", "vue",
        "javascript", "typescript", "tsx", "jsdoc",
        "json", "jsonc", "json5", "jsonnet",
        "http", "sparql", "turtle",
        "terraform",
        "csv", "psv", "tsv",
        "diff",
        "make", "dockerfile", "nix",
        "dot",
        "git_config", "git_rebase", "gitattributes", "gitcommit", "gitignore",
        "gpg",
        "haskell", "haskell_persistent",
        "ini", "toml", "xml", "yaml",
        "java", "kotlin", "properties",
        "latex", "markdown", "markdown_inline",
        "lua", "luadoc", "luap", "luau",
        "nasm",
        "proto",
        "python", "requirements",
        "query",
        "regex",
        "sql",
        "ssh_config",
        "vim", "vimdoc",
    },
    ignore_install = {},
    sync_install = false,
    auto_install = false,
    highlight = {
        enable = true,
        disable = {},
        additional_vim_regex_highlighting = false,
    },
    -- NOTE: See nvim-treesitter-textobjects to extend this
    incremental_selection = {
        enable = true,
        keymaps = {
            init_selection = "<leader>ss",
            node_incremental = "<leader>si",
            scope_incremental = "<leader>sc",
            node_decremental = "<leader>sd",
        },
    },
    indent = {
        enable = true,
        disable = {},
    },
    -- autopairs = {
    --     enable = true,
    -- },
    autotag = {
        enable = true,
        enable_rename = true,
        enable_close = true,
        enable_close_on_slash = true,
    },
 }
