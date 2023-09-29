local module_path = "plugins.lsp"

local prequire = require("helpers.utils").prequire

local lspconfig_status_ok, lspconfig = prequire("lspconfig")
if not lspconfig_status_ok then
    vim.notify("lspconfig not found!")
    return
end

-- Language Servers

local servers = {
    "bashls",   -- Shell
    "cssls",    -- CSS
    "eslint",   -- Eslint
    "hls",      -- Haskell
    "html",     -- HTML
    -- "jdtls", -- also use nvim-jdtls
    "jsonls",   -- JSON
    "lua_ls",   -- Lua
    "tsserver", -- TypeScript/JavaScript
    -- "r_language_server",
    "pyright", -- or jedi_language-server
    "clangd",   -- C / C++
    "cmake",    -- CMake
    -- "arduino_language_server",
    -- "asm_lsp",
    -- "kotlin_language_server",
    -- "sqlls",
    -- voloar or vuels
    -- yamlls
    --
    -- https://github.com/stardog-union/stardog-language-servers/tree/master/packages/sparql-language-server
    -- "turtle_ls", -- (RDF syntax)
    -- "sparql_ls"
}

local default_opts = {
    on_attach = require(module_path .. ".handlers").on_attach,
    capabilities = require(module_path .. ".handlers").capabilities,
    autostart = true,
    single_file_support = true,
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
