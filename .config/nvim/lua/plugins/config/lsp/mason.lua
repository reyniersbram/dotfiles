local mason_status_ok, mason = pcall(require, "mason")
if not mason_status_ok then
    vim.notify("mason not found!")
    return
end

local icons_status_ok, icons = pcall(require, "util.icons")

-- Setup mason
mason.setup {
    -- install_root_dir = require("mason-core.path").concat { vim.fn.stdpath "data", "mason" },
    PATH = "append",
    log_level = vim.log.levels.INFO,
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
        border = icons.ui.window.float.border,
        width = 0.7,
        height = 0.7,
        icons = {
            package_installed = icons_status_ok and icons.ui.status.Done or "✓",
            package_pending = icons_status_ok and icons.ui.status.Loading or "➜",
            package_uninstalled = icons_status_ok and icons.ui.status.Failed or "◍",
        },
        keymaps = {
            toggle_package_expand = "<CR>",
            install_package = "i",
            update_package = "u",
            check_package_version = "c",
            update_all_packages = "U",
            check_outdated_packages = "C",
            uninstall_package = "X",
            cancel_installation = "<C-c>",
            apply_language_filter = "<C-f>",
            toggle_package_intall_log = "<CR>",
            toggle_help = "g?",
        },
    },
}
