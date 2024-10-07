local lspconfig_status_ok, lspconfig = pcall(require, "lspconfig")
if not lspconfig_status_ok then
    vim.notify("lspconfig not found!")
    return
end

-- Language Servers

local servers = {
    "bashls", -- Shell
    "clangd", -- C / C++
    "cmake",  -- CMake
    "cssls",  -- CSS
    "eslint", -- Eslint -> change to eslint_d linter (with none-ls, efm or diagnosticls?)
    "hls",    -- Haskell
    "html",   -- HTML
    "jsonls", -- JSON
    "lua_ls", -- Lua
    "ruff",   -- Python Linter/Formatter
    "taplo",  -- TOML
    "ts_ls",  -- TypeScript/JavaScript
    "volar",  -- Vue

    --
    -- "jdtls", -- also use nvim-jdtls
    -- "r_language_server",
    "pyright", -- or jedi_language-server
    -- "arduino_language_server",
    -- "asm_lsp",
    -- "kotlin_language_server",
    -- "sqlls",
    -- yamlls
    --
    -- https://github.com/stardog-union/stardog-language-servers/tree/master/packages/sparql-language-server
    -- "turtle_ls", -- (RDF syntax)
    -- "sparql_ls"
    -- "kotlin_language_server",
}

require("util").try_with_module(
    "neodev",
    function(neodev)
        neodev.setup {
            library = {
                enabled = true,
                runtime = true,
                types = true,
                plugins = true,
            },
            setup_jsonls = true,
            override = function(root_dir, options) end,
            lspconfig = true,
            pathStrict = true,
        }
    end
)

local lsp = require("core.lsp")
local handlers = require("core.lsp.handlers")
handlers.setup()

local default_opts = {
    on_attach = lsp.on_attach,
    capabilities = handlers.capabilities,
    autostart = true,
    single_file_support = true, -- TODO: necessary?
    handlers = {},              -- TODO: add handlers
}

local opts = {}
for _, server in pairs(servers) do
    opts = default_opts
    local server_opts_status_ok, server_opts =
        pcall(
            require,
            "util.lsp.servers." .. server
        )
    if server_opts_status_ok then
        opts = vim.tbl_deep_extend("force", opts, server_opts)
    end

    lspconfig[server].setup(opts)
end

require("lspconfig.ui.windows").default_options.border = require("util.icons").ui.window.float.border
vim.api.nvim_set_hl(0, "LspInfoBorder", { link = "FloatBorder" })
