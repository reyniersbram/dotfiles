return {
    cmd = { "haskell-language-server-wrapper", "--lsp" },
    filetypes = {"haskell", "lhaskell", "cabal"},
    root_markers = {
        "hie.yaml", "stack.yaml", "cabal.project", "*.cabal", "package.yaml",
    },
    settings = {
        haskell = {
            formattingProvider = "ormolu",
            cabalFormattingProvider = "cabalfmt"
        }
    },
}
