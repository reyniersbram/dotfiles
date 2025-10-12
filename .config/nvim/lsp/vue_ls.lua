local util = require("core.lsp.util")

---@type vim.lsp.Config
return {
    cmd = { "vue-language-server", "--stdio" },
    root_markers = { "package.json" },
    filetypes = { "vue" },
    init_options = {
        typescript = {
            tsdk = "",
        },
        vue = {
            hybridMode = true,
        },
    },
    before_init = function(_, config)
        if config.init_options
            and config.init_options.typescript
            and config.init_options.typescript.tsdk == ""
        then
            config.init_options.typescript.tsdk = util
                .get_typescript_server_path(config.root_dir)
        end
    end,
    on_init = function(client)
        local retries = 0
        local function typescriptHandler(_, result, context)
            local ts_client = vim.lsp.get_clients(
                { bufnr = context.bufnr, name = "ts_ls" }
            )[1]

            if not ts_client then
                if retries <= 100 then
                    retries = retries + 1
                    vim.defer_fn(function()
                        typescriptHandler(_, result, context)
                    end, 100)
                else
                    vim.notify(
                        "Could not find `ts_ls` lsp client, `vue_ls` would not work without it.",
                        vim.log.levels.ERROR
                    )
                end
                return
            end

            local param = unpack(result)
            local id, command, payload = unpack(param)
            ts_client:exec_cmd({
                    title = "vue_request_forward",
                    command = "typescript.tsserverRequest",
                    arguments = {
                        command, payload,
                    }
                },
                { bufnr = context.bufnr }, function(_, r)
                    local response_data = { { id, r and r.body } }
                    client:notify("tsserver/response", response_data)
                end
            )
        end


        client.handlers["tsserver/request"] = typescriptHandler
    end,
}
