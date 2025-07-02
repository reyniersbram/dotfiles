local util = require("core.lsp.util")

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
        if config.init_options and config.init_options.typescript and config.init_options.typescript.tsdk == "" then
            config.init_options.typescript.tsdk = util
                .get_typescript_server_path(config.root_dir)
        end
    end,
}
