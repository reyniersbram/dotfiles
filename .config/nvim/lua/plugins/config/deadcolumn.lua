local colorcolumn_status_ok, colorcolumn = pcall(require, "deadcolumn")
if not colorcolumn_status_ok then
    vim.notify("deadcolumn not found")
    return
end

vim.opt.colorcolumn = "+1"

colorcolumn.setup {
    scope = 'line',
    modes = { 'i', 'ic', 'ix', 'R', 'Rc', 'Rx', 'Rv', 'Rvc', 'Rvx' },
    blending = {
        threshold = 0.80,
        colorcode = '#000000',
        hlgroup = {
            'Normal',
            'background',
        },
    },
    warning = {
        alpha = 0.4,
        offset = 0,
        colorcode = '#FF0000',
        hlgroup = {
            'Error',
            'background',
        },
    },
    extra = {
        follow_tw = "+1",
    },
}
