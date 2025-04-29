local options = {
    autochdir = false,
    autoindent = true,
    autoread = true,
    autowrite = false,
    autowriteall = false,
    backspace = {
        "indent",
        "eol",
        "start",
    },
    backup = false,
    belloff = { "all" },
    clipboard = "unnamedplus",
    cmdheight = 2,
    colorcolumn = "",
    completeopt = {
        "menuone",
        "noselect",
    },
    conceallevel = 0,
    confirm = true,
    cursorcolumn = false,
    cursorline = true,
    cursorlineopt = { "line" },
    expandtab = true,
    exrc = false, -- TODO:
    fileencoding = "utf-8",
    fileformat = "unix",
    fixendofline = true,
    foldenable = false,
    helplang = "en",
    hlsearch = false,
    ignorecase = true,
    inccommand = "nosplit",
    incsearch = true, -- use CTRL-G and CTRL-T to cycle matches
                      -- use CTRL-L to add character of current match
                      -- use CTRL-R,CTRL-W to complete current match
    infercase = true,
    mouse = "a",
    mousefocus = false,
    nrformats = {
        "alpha",
        -- "octal",
        "hex",
        "bin",
    },
    number = true,
    numberwidth = 4,
    pumheight = 10,
    relativenumber = true,
    report = 0,
    scrolloff = 8,
    -- TODO sessionoptions = ...
    shiftround = true,
    shiftwidth = 4,
    showmode = false,
    showtabline = 0,
    sidescrolloff = 8,
    signcolumn = "yes",
    smartcase = true,
    smartindent = true,
    smarttab = true,
    spell = false,
    spelllang = { "en_us", "nl" },
    splitbelow = true,
    splitright = true,
    swapfile = false,
    tabstop = 4,
    termguicolors = true,
    textwidth = 80,
    timeoutlen = 500,
    title = true,
    titlestring = require("util.icons").misc.technology.Neovim .. " NeoVim: %t",
    undofile = true,
    updatetime = 300,
    virtualedit = { "block" },
    wrap = false,
    wrapscan = true,
    writebackup = false,
}

for option, value in pairs(options) do
    vim.opt[option] = value
end

vim.opt.fillchars:append { eob = " " }

vim.opt.formatoptions:remove("t")
vim.opt.formatoptions:append("c")
vim.opt.formatoptions:append("r")
vim.opt.formatoptions:append("o")
vim.opt.formatoptions:append("/")
vim.opt.formatoptions:append("q")
vim.opt.formatoptions:append("w")
-- vim.opt.formatoptions:append("a")
vim.opt.formatoptions:remove("2")
vim.opt.formatoptions:append("n")
vim.opt.formatoptions:append("l")
vim.opt.formatoptions:append("1")
vim.opt.formatoptions:append("j")
vim.opt.formatoptions:remove("p")

vim.opt.shortmess:append("f")
vim.opt.shortmess:append("i")
vim.opt.shortmess:append("l")
vim.opt.shortmess:remove("m")
vim.opt.shortmess:append("n")
vim.opt.shortmess:remove("r")
vim.opt.shortmess:remove("w")
vim.opt.shortmess:append("x")
vim.opt.shortmess:remove("a")
vim.opt.shortmess:append("o")
vim.opt.shortmess:append("O")
vim.opt.shortmess:remove("s")
vim.opt.shortmess:append("t")
vim.opt.shortmess:append("T")
vim.opt.shortmess:remove("W")
vim.opt.shortmess:remove("A")
vim.opt.shortmess:remove("I")
vim.opt.shortmess:append("c")
vim.opt.shortmess:remove("C")
vim.opt.shortmess:remove("q")
vim.opt.shortmess:append("F")
vim.opt.shortmess:remove("S")
