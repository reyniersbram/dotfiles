local status_ok, catppuccin = pcall(require, "catppuccin")
if not status_ok then
    vim.notify("Colorscheme catpuccin not found")
end

catppuccin.setup {
    flavour = "mocha",
    background = {
        light = "latte",
        dark = "mocha",
    },
    transparent_background = false,
    show_end_of_buffer = false,
    term_colors = false,
    dim_inactive = {
        enabled = true,
        shade = "dark",
        percentate = 0.15,
    },
    no_italic = false,
    no_bold = false,
    no_underline = false,
    styles = {
        comments = { "italic" },
        conditionals = { "italic" },
        loops = {},
        functions = {},
        keywords = {},
        strings = {},
        variables = {},
        numbers = {},
        booleans = {},
        properties = {},
        types = {},
        operators = {},
    },
    color_overrides = {},
    custom_highlights = {
        -- TODO: this surely can be done better
        NormalFloat = {
            link = "NormalNC",
        },
        FloatBorder = {
            link = "NormalNC",
        },
    },
    integrations = {
        alpha = true,
        bufferline = true,
        cmp = true,
        gitsigns = true,
        markdown = true,
        mason = true,
        mini = {
            enabled = true,
            indentscope_color = "",
        },
        native_lsp = {
            enabled = true,
            virtual_text = {
                errors = { "italic" },
                hints = { "italic" },
                warnings = { "italic" },
                information = { "italic" },
            },
            underlines = {
                errors = { "underline" },
                hints = { "underline" },
                warnings = { "underline" },
                information = { "underline" },
            },
            inlay_hints = {
                background = true,
            },
        },
        navic = {
            enabled = true,
            custom_bg = "NONE",
        },
        notify = false,
        nvimtree = true,
        treesitter = true,
        treesitter_context = true,
        telescope = {
            enabled = true,
        },
    }
}

vim.cmd.colorscheme("catppuccin")
