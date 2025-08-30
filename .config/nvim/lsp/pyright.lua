return {
    cmd          = { "pyright-langserver", "--stdio" },
    capabilities = {
        workspace = {
            didChangeWatchedFiles = {
                dynamicRegistration = false,
                relativePatternSupport = true
            },
        }
    },
    root_markers = {
        "pyproject.toml",
        "requirements.txt",
        "setup.py",
        "setup.cfg",
        "Pipfile",
        "pyrightconfig.json",
        ".git",
    },
    filetypes    = { "python" },
    settings     = {
        python = {
            analysis = {
                autoSearchPaths = true,
                useLibraryCodeForTypes = true,
                diagnosticMode = "openFilesOnly",
            },
        },
    },
}
