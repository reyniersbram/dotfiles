---@class ClangdInitializeResult: lsp.InitializeResult
---@field offsetEncoding? string

---@type vim.lsp.Config
return {
    cmd = { "clangd" },
    filetypes = { "c", "cpp" },
    root_markers = {
        ".clangd",
        ".clang-tidy",
        ".clang-format",
        "compile_commands.json",
        "compile_flags.txt",
        "configure.ac",
        ".git",
    },
    capabilities = {
        textDocument = {
            completion = {
                editsNearCursor = true,
            },
        },
    },
    offsetEncoding = { "utf-8", "utf-16" },

    ---@param init_result ClangdInitializeResult
    on_init = function(client, init_result)
        if init_result.offsetEncoding then
            client.offset_encoding = init_result.offsetEncoding
        end
    end,
}
