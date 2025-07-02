local vue_language_server_path = "/usr/lib/node_modules/@vue/language-server"

return {
    init_options = {
        hostInfo = "neovim",
        plugins = {
            {
                name = "@vue/typescript-plugin",
                location = vue_language_server_path,
                languages = { "vue", "javascript", "typescript", },
            },
        },
    },
    cmd = { "typescript-language-server", "--stdio" },
    filetypes = {
        "javascript", "typescript",
        "vue",
    },
    root_markers = { "jsconfig.json, package.json", ".git" },
    handlers = {
        ["_typescript_rename"] = function(_, result, ctx)
            local client = assert(vim.lsp.get_client_by_id(ctx.client_id))
            vim.lsp.util.show_document({
                uri = result.textDocument.uri,
                range = {
                    start = result.position,
                    ["end"] = result.position,
                },
            }, client.offset_encoding)
            vim.lsp.buf.rename()
            return vim.NIL
        end,
    },
    on_attach = function(client)
        -- ts_ls provides `source.*` code actions that apply to the whole file. These only appear in
        -- `vim.lsp.buf.code_action()` if specified in `context.only`.
        vim.api.nvim_buf_create_user_command(0, "LspTypescriptSourceAction",
            function()
                local source_actions = vim.tbl_filter(function(action)
                        return vim.startswith(action, "source.")
                    end,
                    client.server_capabilities.codeActionProvider
                    .codeActionKinds)

                vim.lsp.buf.code_action({
                    context = {
                        only = source_actions,
                    },
                })
            end, {})
    end,
}
