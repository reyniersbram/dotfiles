local lspconfig_status_ok, lspconfig = pcall(require, "lspconfig")
if not lspconfig_status_ok then
    vim.notify("lspconfig not found!")
    return
end

-- Language Servers

local servers = {
    "bashls",   -- Shell
    "clangd",   -- C / C++
    "cmake",    -- CMake
    "cssls",    -- CSS
    "eslint",   -- Eslint
    "hls",      -- Haskell
    "html",     -- HTML
    "jsonls",   -- JSON
    "lua_ls",   -- Lua
    "tsserver", -- TypeScript/JavaScript
    --
    -- "jdtls", -- also use nvim-jdtls
    -- "r_language_server",
    -- "pyright", -- or jedi_language-server
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
    on_attach = require("plugins.lsp.handlers").on_attach,
    capabilities = require("plugins.lsp.handlers").capabilities,
    autostart = true,
    single_file_support = true,
}

local opts = {}

for _, server in pairs(servers) do
    opts = default_opts
    local server_opts_status_ok, server_opts =
        pcall(
            require,
            "plugins.lsp.settings." .. server
        )
    if server_opts_status_ok then
        opts = vim.tbl_deep_extend("force", opts, server_opts)
    end
    lspconfig[server].setup(opts)
end
