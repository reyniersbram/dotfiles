local mason_registry = require('mason-registry')
local vue_language_server_path = mason_registry.get_package('vue-language-server'):get_install_path() ..
    '/node_modules/@vue/language-server'

return {
    cmd = { "typescript-language-server", "--stdio" },
    filetypes = {
        "javascript", "typescript",
        "javascriptreact", "typescriptreact",
        "javascript.jsx", "typescript.tsx",
        "vue", "json"
    },
    init_options = {
        hostInfo = "neovim",
        plugins = {
            {
                name = "@vue/typescript-plugin",
                location = vue_language_server_path,
                languages = { "javascript", "typescript", "vue" },
            },
        },
    },
}