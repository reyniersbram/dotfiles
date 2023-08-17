local lspconfig = require("lspconfig")

return {
    cmd = {
        "haskell-language-server-wrapper",
        "--lsp"
    },
    filetypes = { "haskell", "lhaskell", "cabal" },
    root_dir =
    function(path)
        return (
            lspconfig.util.root_pattern("hie.yaml", 'stack.yaml', 'cabal.project')(path)
            or lspconfig.util.root_pattern("*.cabal", "package.yaml")(path)
        )
    end,
    settings = {
        haskell = {
            cabalFormattingProvider = "cabalfmt",
            formattingProvider = "ormolu",
        },
    },
}
