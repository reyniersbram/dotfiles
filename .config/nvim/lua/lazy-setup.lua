local plugin_path = vim.fn.stdpath("data") .. "/lazy"

--- Install lazy.nvim
---@param install_path string The target path for the intallation
local function bootstrap(install_path)
    local repo = "https://github.com/folke/lazy.nvim.git"
    local output = vim.fn.system {
        "git",
        "clone",
        "--filter=blob:none",
        "--branch=stable",
        repo,
        install_path,
    }
    if vim.v.shell_error ~= 0 then
        vim.api.nvim_echo({
            { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
            { out, "WarningMsg" },
            { "\nPress any key to exit..." },
        }, true, {})
        vim.fn.getchar()
        os.exit(1)
    end
end

local install_path = plugin_path .. "/lazy.nvim"
if not vim.uv.fs_stat(install_path) then
    bootstrap(install_path)
end
vim.opt.rtp:prepend(install_path)


local lazy = require "lazy"
local Util = require "lazy.core.util"
local icons = require "util.icons"

lazy.setup {
    root = plugin_path,
    defaults = {
        lazy = false,
        version = false,
        cond = nil,
    },
    spec = {
        import = "plugins",
    },
    local_spec = true,
    lockfile = vim.fn.stdpath("config") .. "/lazy-lock.json",
    concurrency = jit.os:find("Windows") and (vim.uv.available_parallelism() * 2) or nil,
    git = {
        log = { "-8" },
        timeout = 120,
        url_format = "https://github.com/%s.git",
        filter = true,
        throttle = {
            enable = false,
            rate = 2,
            duration = 5000,
        },
        cooldown = 0,
    },
    pkg = {
        enabled = true,
        cache = vim.fn.stdpath("state") .. "/lazy/pkg-cache.lua",
        sources = {
            "lazy",
            "rockspec",
            "packspec",
        },
    },
    rocks = {
        enabled = true,
        root = vim.fn.stdpath("data") .. "/lazy-rocks",
        server = "https://nvim-neorocks.github.io/rocks-binaries/",
        hererocks = nil,
    },
    dev = {
        path = "~/Projects",
        patterns = {},
        fallback = false,
    },
    install = {
        missing = true,
        colorscheme = { "default" },
    },
    ui = {
        size = { width = 0.7, height = 0.7 },
        wrap = true,
        border = icons.ui.border,
        backdrop = 60,
        title = nil,
        title_pos = "center",
        pills = true,
        icons = { -- TODO:
            cmd = " ",
            config = icons.ui.settings,
            event = " ",
            favorite = " ",
            ft = icons.files.file_code,
            init = " ",
            import = icons.files.file_import,
            keys = icons.misc.keyboard,
            lazy = "󰒲 ",
            loaded = "●",
            not_loaded = "○",
            plugin = icons.programming.completion_item_kind.Module,
            runtime = icons.misc.tech.Vim,
            require = icons.misc.programming.language.lua,
            source = " ",
            start = " ",
            task = "✔ ",
            list = {
                "●",
                "➜",
                "★",
                "‒",
            },
        },
        browser = nil,
        throttle = 1000 / 30,
        custom_keys = { -- TODO:
            -- You can define custom key maps here. If present, the description will
            -- be shown in the help menu.
            -- To disable one of the defaults, set it to false.

            ["<localleader>l"] = {
                function(plugin)
                    require("lazy.util").float_term({ "lazygit", "log" }, {
                        cwd = plugin.dir,
                    })
                end,
                desc = "Open lazygit log",
            },

            ["<localleader>i"] = {
                function(plugin)
                    Util.notify(vim.inspect(plugin), {
                        title = "Inspect " .. plugin.name,
                        lang = "lua",
                    })
                end,
                desc = "Inspect Plugin",
            },

            ["<localleader>t"] = {
                function(plugin)
                    require("lazy.util").float_term(nil, {
                        cwd = plugin.dir,
                    })
                end,
                desc = "Open terminal in plugin dir",
            },
        },
    },
    headless = {
        process = true,
        log = true,
        task = true,
        colors = true,
    },
    diff = {
        cmd = "git",
    },
    checker = {
        enabled = true,
        concurrency = nil,
        notify = true,
        frequency = 3600,
        check_pinned = false,
    },
    change_detection = {
        enabled = true,
        notify = true,
    },
    performance = {
        cache = {
            enabled = true,
        },
        reset_packpath = true,
        rtp = {
            reset = true,
            paths = {},
            disabled_plugins = {
                -- TODO:
                "netrwPlugin",
            },
        },
    },
    readme = {
        enabled = true,
        root = vim.fn.stdpath("state") .. "/lazy/readme",
        files = { "README.md", "lua/**/README.md" },
        skip_if_doc_exists = true,
    },
    state = vim.fn.stdpath("state") .. "/lazy/state.json",
    profiling = {
        loader = false,
        require = false,
    },
}
