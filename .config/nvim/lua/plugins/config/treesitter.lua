local ts_status_ok, treesitter = pcall(require, "nvim-treesitter.configs")
if not ts_status_ok then
    vim.notify("treesitter not found")
    return
end

treesitter.setup {
    ensure_installed = "all",
    sync_install = false,
    ignore_install = { "" }, -- List of parsers to ignore installing
    autopairs = {
        enable = true,
    },
    highlight = {
        enable = true, -- false will disable the whole extension
        disable = { "" }, -- list of language that will be disabled
        additional_vim_regex_highlighting = false,
    },
    indent = {
        enable = true,
        disable = { "" }
    },
 }
