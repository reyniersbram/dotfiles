local function bootstrap()
    local install_path = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
    if not vim.loop.fs_stat(install_path) then
        vim.fn.system {
            "git",
            "clone",
            "--filter=blob:none",
            "https://github.com/folke/lazy.nvim.git",
            "--branch=stable",
            install_path,
        }
    end
    vim.opt.rtp:append(install_path)
end

bootstrap()

local lazy_status_ok, lazy = pcall(require, "lazy")
if not lazy_status_ok then
    vim.notify("Could not load lazy.nvim")
end

lazy.setup("plugins", require("plugins.config.lazy"))
