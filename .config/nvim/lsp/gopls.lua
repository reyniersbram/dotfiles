return {
    cmd = { "gopls" },
    root_markers = { "go.work", "go.mod", ".git" },
    filetypes = { "go", "gomod", "gowork", "gotmpl" },
    settings = {
        gopls = {
            gofumpt = true,
            codelenses = {
                -- Go
                generate = true,
                regenerate_cgo = true,
                test = true,
                -- go.mod
                run_govulncheck = true,
                vulncheck = false,
                tidy = true,
                upgrade_dependency = true,
                vendor = true,
            },
            semanticTokens = true,
            usePlaceholders = true,
            matcher = "Fuzzy",
            -- TODO: find analyzers I want, should they be here or project specific?
            -- analyses = {},
            diagnosticsTrigger = "Edit",
            analysisProgressReporting = true,
            hoverKind = "FullDocumentation",
            linksInHover = true,
            hints = {
                assignVariableTypes = true,
                compositeLiteralFields = true,
                compositeLiteralTypes = true,
                constantValues = true,
                functionTypeParameters = true,
                parameterNames = true,
                rangeVariableTypes = true,
            },
        },
    },
}
