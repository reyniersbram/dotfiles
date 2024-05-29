return {
    cmd = { "taplo", "lsp", "stdio" },
    filetypes = { "toml" },
    single_file_support = true,
    settings = {
        evenBetterToml = {
            formatter = {
                alignEntries = true,
                alignComments = true,
                arrayTrailingComma = true,
                arrayAutoExpand = true,
                arrayAutoCollapse = true,
                compactArrays = true,
                compactInlineTables = false,
                inlineTableExpand = true,
                compactEntries = false,
                columnWidth = 80,
                indentTables = false,
                indentEntries = false,
                indentString = "    ",
                trailingNewline = true,
                reorderKeys = false,
                reorderArrays = false,
                allowedBlankLines = 2,
                crlf = false,
            },
        },
    },
}
