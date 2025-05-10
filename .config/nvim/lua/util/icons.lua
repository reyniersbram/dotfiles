local M = {}

M.diagnostics = {
    [vim.diagnostic.severity.ERROR] = "\u{ea87}", -- 
    [vim.diagnostic.severity.WARN] = "\u{ea6c}",  -- 
    [vim.diagnostic.severity.INFO] = "\u{ea74}",  -- 
    [vim.diagnostic.severity.HINT] = "\u{eb32}",  -- 
}

M.files = {
    file = "\u{f0214}",           -- 󰈔
    files = "\u{f0222}",          -- 󰈢
    file_import = "\u{f0220}",    -- 󰈠
    file_code = "\u{f022e}",      -- 󰈮
    symlink = "\u{f1031}",        -- 󱀱
    folder = {
        folder = "\u{f024b}",     -- 󰉋
        open = "\u{f0770}",       -- 󰝰
        empty = "\u{f0256}",      -- 󰉖
        empty_open = "\u{f0dcf}", -- 󰷏
        symlink = "\u{f19ef}",    -- 󱧯
    },
}

M.misc = {
    gear = "\u{f013}",     -- 
    keyboard = "\u{f11c}", -- 
    programming = {
        language = {
            lua = "\u{e826}", -- 
        },
    },
    tech = {
        Neovim = "\u{f36f}", -- 
        Vim = "\u{e7c5}",    -- 
    },
    telescope = "\u{f46b}",  -- 
}

M.programming = {
    -- Contains the CompletionItemKinds from the LSP specification
    completion_item_kind = {
        Text = "\u{e612}",          -- 
        Method = "\u{ea8c}",        -- 
        Function = "\u{f0871}",     -- 󰡱
        Constructor = "\u{eb6d}",   -- 
        Field = "\u{eb5f}",         -- 
        Variable = "\u{ea88}",      -- 
        Class = "\u{eb5b}",         -- 
        Interface = "\u{f0e8}",     -- 
        Module = "\u{eb29}",        -- 
        Property = "\u{eb65}",      -- 
        Unit = "\u{e21f}",          -- 
        Value = "value",            -- TODO
        Enum = "\u{f15d}",          -- 
        Keyword = "[keyword]",      -- TODO
        Snippet = "\u{f0c4}",       -- 
        Color = "\u{eb5c}",         -- 
        File = "\u{eae9}",          -- 
        Reference = "\u{eb36}",     -- 
        Folder = "\u{ea83}",        -- 
        EnumMember = "\u{eb5e}",    --  -- TODO:
        Constant = "\u{eb5d}",      --  -- TODO:
        Struct = "\u{ea91}",        -- 
        Event = "\u{ea86}",         -- 
        Operator = "\u{eb64}",      -- 
        TypeParameter = "\u{ea92}", -- 

    },
    git = {
        add = "\u{f457}",        -- 
        mod = "\u{f459}",        -- 
        remove = "\u{f458}",     -- 
        ignore = "\u{f474}",     -- 
        rename = "\u{f45a}",     -- 
        diff = "\u{f440}",       -- 
        untracked = "\u{f15e3}", -- 󱗣
        repo = "\u{f401}",       -- 
        merge = "\u{e727}",      -- 
        branch = "\u{e725}",     -- 
        commit = "\u{e729}",     -- 
    },
}

M.ui = {
    border = "rounded",
    select = "\u{f0da}", -- 
    settings = M.misc.gear,
}

return M
