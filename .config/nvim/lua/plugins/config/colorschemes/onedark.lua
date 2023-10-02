local status_ok, onedark = pcall(require, "onedark")
if not status_ok then
    vim.notify("Colorscheme onedark not found!")
    return
end
onedark.setup{
    style = "darker",
}
onedark.load()
