-- Neovim options
-- This covers the bulk of all Neovim options
-- For help about an option: `:help *option*` or `:options`
-- The sorting is based on the option-window ordering
-- HINT: `set kp=":help"` to use the help page for the K command

local options = {}

options.general_opts = {
    autochdir = false,
    backspace = {
        "eol",
        "indent",
        "nostop",
    },
    clipboard = { "unnamedplus" },
    exrc = false, -- TODO: look into usage
    inccommand = "nosplit",
    -- TODO: jumpoptions
    nrformats = {
        "alpha",
        "hex",
        "bin",
    },
    timeout = true,
    timeoutlen = 500,
}
vim.opt.sessionoptions:append("blank")
vim.opt.sessionoptions:append("buffers")
vim.opt.sessionoptions:append("folds")
vim.opt.sessionoptions:append("help")
vim.opt.sessionoptions:append("localoptions")
vim.opt.sessionoptions:remove("curdir")
vim.opt.sessionoptions:append("sesdir")
vim.opt.sessionoptions:append("tabpages")
vim.opt.sessionoptions:append("terminal")
vim.opt.sessionoptions:append("winsize")
vim.opt.sessionoptions:append("globals")
vim.opt.sessionoptions:append("options")

vim.opt.sessionoptions:remove("skiprtp")
vim.opt.sessionoptions:remove("resize")
vim.opt.sessionoptions:remove("winpos")

vim.opt.virtualedit:append("block")

options.cmd_opts = {
    cmdwinheight = 7,
    fileignorecase = true,
    wildignorecase = true,
    wildmenu = true,
}
vim.opt.wildoptions:append("fuzzy")
vim.opt.wildoptions:append("pum")

options.color_opts = {
    background = "dark", -- TODO: do this somewhere else
    colorcolumn = { "+1" },
    hlsearch = false,
    termguicolors = true,
}

options.complete_opts = {
    completeopt = {
        "menu",
        -- "preview", -- TODO: test what this does
        "noselect",
        -- "popup", -- TODO: test what this does
    },
    infercase = false,
    showmatch = false,
    pumblend = 15,
    pumheight = 10,
}

options.cursor_opts = {
    cursorcolumn = false,
    cursorline = true,
    cursorlineopt = {
        "line",
        "number",
    },
}

-- TODO:
options.diff_opts = {
}

options.display_opts = {
    breakindent = true,
    cmdheight = 2,
    linebreak = true,
    list = true,
    number = true,
    numberwidth = 4,
    relativenumber = true,
    scrolloff = 8,
    showbreak = "",
    sidescrolloff = 8,
    signcolumn = "yes:1",
    smoothscroll = true,
    wrap = false,
}
vim.opt.fillchars = vim.tbl_extend(
    "force",
    vim.opt.fillchars:get(), {
        eob = " ",
    }
)
vim.opt.listchars = vim.tbl_extend(
    "force",
    vim.opt.listchars:get(), {
        tab = "  ",
        trail = "~",
    }
)

options.file_opts = {
    autoread = true,
    autowrite = true,
    autowriteall = true,
    backup = false,
    swapfile = true,
    undofile = true,
    updatetime = 500,
    writebackup = false,
}

-- TODO:
options.fold_opts = {
}

options.format_opts = {
    -- TODO: cindent, cinkeys, cinoptions, cinscopedecls, cinwords
    -- TODO: copyindent
    autoindent = true,
    bomb = false,
    expandtab = true,
    encoding = "utf-8",
    fileencoding = "utf-8",
    fileformat = "unix",
    fixendofline = true,
    shiftround = true,
    shiftwidth = 4,
    smartindent = true,
    smarttab = true,
    softtabstop = -1,
    tabstop = 8,
    textwidth = 80,
}
-- TODO: formatoptions

-- TODO:
options.make_opts = {
}

options.message_opts = {
    belloff = { "all" },
    confirm = true,
    report = 0,
    showcmd = true,
    showcmdloc = "last",
    showmode = true,
}
vim.opt.shortmess:append("l")
vim.opt.shortmess:append("m")
vim.opt.shortmess:append("r")
vim.opt.shortmess:append("w")
vim.opt.shortmess:append("o")
vim.opt.shortmess:append("O")
vim.opt.shortmess:append("s")
vim.opt.shortmess:append("t")
vim.opt.shortmess:append("T")
vim.opt.shortmess:append("c")

vim.opt.shortmess:remove("W")
vim.opt.shortmess:remove("A")
vim.opt.shortmess:remove("I")
vim.opt.shortmess:remove("C")
vim.opt.shortmess:remove("q")
vim.opt.shortmess:remove("F")
vim.opt.shortmess:remove("S")

options.mouse_opts = {
    mouse = "a",
    mousefocus = false,
    mousemodel = "popup_setpos",
    mousescroll = "ver:3",
}

options.search_opts = {
    ignorecase = true,
    incsearch = true,
    smartcase = true,
    wrapscan = true,
}

-- TODO:
options.spell_opts = {
    spell = false,
}

-- TODO:
options.tab_opts = {
}

-- TODO:
options.tag_opts = {
    tagcase = "followscs",
}

options.title_opts = {
    icon = true,
    iconstring = require("util.icons").misc.tech.Neovim,
    -- TODO: configure title
    title = true,
    titlestring = require("util.icons").misc.tech.Neovim .. " %t",
}

options.window_opts = {
    eadirection = "both",
    equalalways = true,
    hidden = false, -- TODO: look into this
    splitbelow = true,
    splitkeep = "cursor",
    splitright = true,
    -- TODO: switchbuf
    -- TODO: set winwidth and winheight
    winblend = 15,
}

for _, option_map in pairs(options) do
    for option, value in pairs(option_map) do
        vim.opt[option] = value
    end
end

if vim.fn.has("browse") == 1 then
    vim.opt.browsedir = "last"
end

-- TODO: matchpairs for html, vue,...
-- vim.opt.matchpairs:append("<:>")

-- TODO: should this be here?
vim.api.nvim_create_autocmd("TermOpen", {
    callback = function()
        vim.opt_local.number = false
        vim.opt_local.relativenumber = false
        vim.opt.signcolumn = "no"
    end
})

