local navic_status_ok, navic = pcall(require, "nvim-navic")
if not navic_status_ok then
    vim.notify("navic not found")
    return
end

local icons_status_ok, icons = pcall(require, "util.icons")
if not icons_status_ok then
    vim.notify("icons not found")
    return
end

local kind_icons = {}
for k, v in pairs(icons.kind) do
    kind_icons[k] = v .. " "
end

navic.setup {
    icons = kind_icons,
    higlight = true,
    separator = " " .. ">" .. " ",
    depth_limit = 4,
    depth_limit_indicator = icons.misc.Ellipsis,
    safe_output = true,
    click = true,
    lazy_update_context = false,
    lsp = {
        auto_attach = true,
        preference = {},
    },
}
