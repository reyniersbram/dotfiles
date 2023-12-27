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

navic.setup({
    icons = kind_icons,
    lsp = {
        auto_attach = true,
        preference = {},
    },
    higlight = true,
    separator = " " .. ">" .. " ",
    depth_limit = 0,
    depth_limit_indicator = "..",
    safe_output = true,
    click = true,
})
