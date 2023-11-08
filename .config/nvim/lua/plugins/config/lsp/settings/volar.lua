return {
    cmd = { "vue-language-server", "--stdio" },
    filetypes = {
        'typescript',
        'javascript',
        'javascriptreact',
        'typescriptreact',
        'vue',
        'json'
    },
    init_options = {
        typescript = {
            tsdk = "",
        },
    },
}
