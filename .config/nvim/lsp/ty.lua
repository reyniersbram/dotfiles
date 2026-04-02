---@type vim.lsp.Config
return {
    cmd = { "ty", "server" },
    root_markers = {
        "ty.toml",
        "pyproject.toml",
        "setup.py",
        "setup.cfg",
        "requirements.txt",
        ".git"
    },
    filetypes = { "python" },
    settings = {
        ty = {
            diagnosticMode = "openFilesOnly",
            inlayHints = {
                variableTypes = true,
                callArgumentNames = true,
            },
            completions = {
                autoImport = true,
            },
        },
    },
}
