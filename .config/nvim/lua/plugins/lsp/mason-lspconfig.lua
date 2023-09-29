local mason_lspconfig_status_ok, mason_lspconfig = pcall(require, "mason-lspconfig")
if not mason_lspconfig_status_ok then
    vim.notify("mason-lspconfig not found!")
    return
end

-- Setup mason-lspconfig
mason_lspconfig.setup {
    ensure_installed = nil,
    automatic_installation = { exclude = { "hls", "clangd" } },
    handlers = nil,
}
