return {
    cmd = { "vscode-css-language-server", "--stdio" },
    filetype = { "css", "scss", "less" },
    settings = {
        css = {
            validate = true,
        },
        less = {
            validate = true,
        },
        scss = {
            validate = true,
        },
    },
}
