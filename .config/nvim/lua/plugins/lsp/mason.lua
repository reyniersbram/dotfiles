local mason_status_ok, mason = pcall(require, "mason")
if not mason_status_ok then
    vim.notify("mason not found!")
    return
end

local mason_lspconfig_status_ok, mason_lspconfig = pcall(require, "mason-lspconfig")
if not mason_lspconfig_status_ok then
    vim.notify("mason-lspconfig not found!")
    return
end

local icons_status_ok, icons = pcall(require, "helpers.icons")

-- Setup mason
mason.setup {
    max_concurrent_installers = 5,
    registries = {
        "github:mason-org/mason-registry",
    },
    providers = {
        "mason.providers.registry-api",
        "mason.providers.client",
    },
    github = {
        download_url_template = "https://github.com/%s/releases/download/%s/%s",
    },
    pip = {
        upgrade_pip = false,
        install_args = {},
    },
    ui = {
        check_outdated_packages_on_open = true,
        -- border options: none, single, double, rounded, solid, shadow, {...}
        border = "rounded",
        width = 0.8,
        height = 0.9,
        icons = {
            package_installed = icons_status_ok and icons.ui.status.Done or "✓",
            package_pending = icons_status_ok and icons.ui.status.Loading or "➜",
            package_uninstalled = icons_status_ok and icons.ui.status.Failed or "◍",
        },
    },
    keymaps = {
        install_package = "i",
        update_package = "u",
        uninstall_package = "X",
        update_all_packages = "U",
        toggle_package_expand = "<CR>",
        check_package_version = "c",
        check_outdated_packages = "C",
        cancel_installation = "<C-c>",
        apply_language_filter = "<C-f>",
    },
}

-- Setup mason-lspconfig
mason_lspconfig.setup {
    ensure_installed = {

    },
    automatic_installation = false,
    handlers = nil,
}
