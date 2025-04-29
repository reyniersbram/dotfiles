return {
    cmd = { "ruff", "server", "--preview" },
    filetypes = { "python" },
    single_file_support = true,
    init_options = {
        settings = {
            configurationPreference = "editorFirst",
            fixAll = true,
            organizeImports = true,
            showSyntaxErrors = true,
            codeAction = {
                disableRuleComment = { enable = true },
                fixViolations = { enable = true },
            },
            lint = {
                enable = true,
            }
        }
    }
}
