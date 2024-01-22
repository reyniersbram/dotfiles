local icons = require("util.icons")

local autocmd_group = vim.api.nvim_create_augroup("LazyAutocommit", { clear = true })
local function attach_autocommit(event, message)
    vim.api.nvim_create_autocmd("User", {
        group = autocmd_group,
        pattern = event,
        desc = "Commit lazy-lock.json after " .. event,
        callback = function()
            vim.fn.jobstart(
                "git add lazy-lock.json && git commit -m '[lazy.nvim] " .. message .. "'",
                {
                    cwd = vim.fn.stdpath("config"),
                }
            )
        end,
    })
end

attach_autocommit("LazyUpdate", "plugin(s) updated")
attach_autocommit("LazyInstall", "new plugin(s) installed")
attach_autocommit("LazyClean", "unused plugin(s) removed")

return {
    root = vim.fn.stdpath("data") .. "/lazy",
    defaults = {
        lazy = false,
        version = nil,
        cond = nil,
    },
    spec = nil,
    lockfile = vim.fn.stdpath("config") .. "/lazy-lock.json",
    -- concurrency = jit.os:find("Windows") and (vim.loop.available_parallelism() * 2) or nil,
    git = {
        log = { "-8" }, -- show commits from the last 3 days
        timeout = 120,  -- kill processes that take more than 2 minutes
        url_format = "https://github.com/%s.git",
        filter = true,
    },
    dev = {
        path = "~/Projects",
        patterns = {},
        fallback = false,
    },
    install = {
        missing = true,
        colorscheme = { "habamax" },
    },
    ui = {
        size = { width = 0.7, height = 0.7 },
        wrap = true,
        border = icons.ui.window.float.border,
        title = nil,
        title_pos = "center",
        pills = true,
        icons = {
            cmd = icons.ui.Terminal .. " ",
            config = icons.ui.Gear .. " ",
            event = icons.kind.Event .. " ",
            ft = icons.documents.File .. " ",
            init = " ",
            import = icons.documents.ImportFile .. " ",
            keys = icons.ui.Keyboard .. " ",
            lazy = icons.ui.Sleep .. " ",
            loaded = icons.ui.BigCircle .. " ",
            not_loaded = icons.ui.BigUnfilledCircle .. " ",
            plugin = icons.ui.Package .. " ",
            runtime = icons.misc.technology.Vim .. " ",
            source = icons.ui.Code .. " ",
            start = icons.ui.Start .. " ",
            task = icons.ui.CheckBox.Check .. " ",
            list = {
                "●",
                "➜",
                "★",
                "‒",
            },
        },
        browser = nil,
        throttle = 20,
        custom_keys = {
            -- you can define custom key maps here.
            -- To disable one of the defaults, set it to false

            -- open lazygit log
            -- ["<localleader>l"] = function(plugin)
            --     require("lazy.util").float_term({ "lazygit", "log" }, {
            --     cwd = plugin.dir,
            -- })
            -- end,

            -- open a terminal for the plugin dir
            -- ["<localleader>t"] = function(plugin)
            --     require("lazy.util").float_term(nil, {
            --     cwd = plugin.dir,
            -- })
            -- end,
        },
    },
    diff = {
        -- diff command <d> can be one of:
        -- * browser: opens the github compare view. Note that this is always mapped to <K> as well,
        --   so you can have a different command for diff <d>
        -- * git: will run git diff and open a buffer with filetype git
        -- * terminal_git: will open a pseudo terminal with git diff
        -- * diffview.nvim: will open Diffview to show the diff
        cmd = "git",
    },
    checker = {
        enabled = true,
        concurrency = nil,
        notify = true,
        frequency = 3600,
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
                -- "gzip",
                -- "matchit",
                -- "matchparen",
                -- "netrwPlugin",
                -- "tarPlugin",
                -- "tohtml",
                -- "tutor",
                -- "zipPlugin",
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
    build = {
        warn_on_override = true,
    },
}
