local function config()
    local colorscheme = "onedark"

    local status_ok, scheme = pcall(require, colorscheme)
    if not status_ok then
        vim.notify("Colorscheme " .. colorscheme .. " not found!")
        return
    end
    scheme.setup{
        style = "darker",
    }
    scheme.load()
end

return {
    {
        "ellisonleao/gruvbox.nvim",
        lazy = true,
        priority = 100,
    },
    {
        "navarasu/onedark.nvim",
        lazy = false,
        priority = 100,
        config = config,
    },
}
