local module_path = "plugins.lsp"

require(module_path .. ".native")
require("plugins.lsp.mason")
require("plugins.lsp.mason-lspconfig")
require(module_path .. ".lspconfig")
-- require("config.lsp.lsp-signature")
-- require("config.lsp.null-ls")
