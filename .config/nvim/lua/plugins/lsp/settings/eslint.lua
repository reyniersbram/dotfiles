return {
    cmd = { "vscode-eslint-language-server", "--stdio" },
    filetypes = {
        "javascript", "javascriptreact", "javascript.jsx",
        "typescript", "typescriptreact", "typescript.tsx",
        "vue", "svelte", "astro",
    },
    -- see https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#eslint
    -- settings = {
    --
    -- }
}
