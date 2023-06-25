local module_path = "plugins.lsp"

local prequire = require("helpers.utils").prequire

local lspconfig_status_ok, lspconfig = prequire("lspconfig")
if not lspconfig_status_ok then
    vim.notify("lspconfig not found!")
    return
end

-- Language Servers

local servers = {
    "lua_ls",
    "hls",
    "bashls",
    -- "r_language_server",
    -- "pyright",
    -- "tsserver",
    -- "clangd",
    -- "html",
}

local default_opts = {
    on_attach = require(module_path .. ".handlers").on_attach,
    capabilities = require(module_path .. ".handlers").capabilities,
    autostart = true,
}

local opts = {}

for _, server in pairs(servers) do
    opts = default_opts
    local server_opts_status_ok, server_opts =
        prequire(
            module_path .. ".settings." .. server
        )
    if server_opts_status_ok then
        opts = vim.tbl_deep_extend("force", opts, server_opts)
    end
    lspconfig[server].setup(opts)
end
