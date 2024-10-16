M = {
    kind = {
        Text = "\u{e612}",          -- 
        Method = "\u{ea8c}",        -- 
        Function = "\u{f0871}",     -- 󰡱
        Constructor = "\u{eb6d}",   -- 
        Field = "\u{eb5f}",         -- 
        Variable = "\u{ea88}",      -- 
        Class = "\u{eb5b}",         -- 
        Interface = "\u{f0e8}",     -- 
        -- Module = "\u{eb29}", --  -- TODO:
        Module = "\u{ea8c}",        -- 
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

        -- TODO: these are kinds in nvim-navic
        NameSpace = "[NS]", -- "\u{ea8b}", -- 
        Package = "[P]",    -- "\u{eb29}", -- 
        String = "[S]",     -- "\u{ea93}", -- 
        Number = "[N]",
        "\u{ea90}",         -- 
        -- Boolean = "\u{ea8f}", -- 
        -- Array = "\u{ea8a}", -- 
        -- Object = "\u{ea8b}", -- 
        -- Key = "\u{eb11}", -- 
        -- Null = "\u{f07e2}", -- 󰟢
    },
    type = {
        Array = "\u{ea8a}",   -- 
        Object = "\u{ea8b}",  -- 
        String = "\u{ea93}",  -- 
        Number = "\u{ea90}",  -- 
        Boolean = "\u{ea8f}", -- 
    },
    documents = {
        File = "\u{f0214}",          -- 󰈔
        Files = "\u{f0222}",         -- 󰈢
        ImportFile = "\u{f0220}",    -- 󰈠
        LockedFile = "\u{f0221}",    -- 󰈡
        Folder = {
            Folder = "\u{f024b}",    -- 󰉋
            Open = "\u{f0770}",      -- 󰝰
            Empty = "\u{f0256}",     -- 󰉖
            EmptyOpen = "\u{f0dcf}", -- 󰷏
            SymLink = "\u{f19ef}",   -- 󱧯
            Collapsed = " ",
            Expanded = " ",
        },
        Indent = {
            Corner = "└",
            Edge = "│",
            Item = "├",
            Bottom = "─",
            None = " ",
        },
        SymLink = "\u{f1031}", -- 󱀱
        SymLinkArrow = ""
    },
    git = {
        Add = "\u{f457}",        -- 
        Mod = "\u{f459}",        -- 
        Remove = "\u{f458}",     -- 
        Ignore = "\u{f474}",     -- 
        Rename = "\u{f45a}",     -- 
        Diff = "\u{f440}",       -- 
        Untracked = "\u{f15e3}", -- 󱗣
        Repo = "\u{f401}",       -- 
        Merge = "\u{e727}",      -- 
        Branch = "\u{e725}",     -- 
        Commit = "\u{e729}",     -- 
    },
    ui = {
        Lock = " ",
        Circle = " ",
        BigCircle = " ",
        BigUnfilledCircle = " ",
        Close = " ",
        NewFile = " ",
        Search = " ",
        Lightbulb = " ",
        Project = " ",
        Dashboard = " ",
        History = " ",
        Comment = " ",
        Code = "\u{f121}", -- 
        Telescope = " ",
        Gear = " ",
        Package = " ",
        List = " ",
        SignIn = " ",
        SignOut = " ",
        NoteBook = " ",
        Check = " ",
        Fire = " ",
        Note = " ",
        BookMark = " ",
        Pencil = " ",
        ChevronRight = "", -- eab6
        Table = " ",
        Calendar = " ",
        CloudDownload = " ",
        Terminal = "\u{f489}",   -- 
        Keyboard = "\u{f030c}",  -- 󰌌
        Sleep = "\u{f04b2}",     -- 󰒲
        Start = "\u{ead3}",      -- 
        CheckBox = {
            Box = "\u{f0131}",   -- 󰄱
            Check = "\u{f0135}", -- 󰄵
        },
        status = {
            Loading = "\u{f0453}", -- 󰑓
            Failed = "✗ ",
            Done = "✓ ",
        },
        window = {
            float = {
                border = { "╭", "─", "╮", "│", "╯", "─", "╰", "│" },
            },
        },
        spinner_frames = { "⣾", "⣽", "⣻", "⢿", "⡿", "⣟", "⣯", "⣷" },
    },
    misc = {
        Robot = " ",
        Squirrel = " ",
        Tag = " ",
        Watch = " ",
        Ellipsis = "\u{2026}",     -- …
        OS = {
            LinuxTux = "\u{e712}", -- 
            Apple = "\u{e711}",    -- 
            Windows = "\u{e70f}",  -- 
        },
        technology = {
            Vim = "\u{e7c5}",    -- 
            Neovim = "\u{f36f}", -- 
        },
        keyboard = {
            Space = "󱁐 ",
            Tab = "󰌒 "
        }
    },
}

M.diagnostics = {
    ERROR = "", -- TODO: find proper icon
    WARN = "", -- TODO: find proper icon
    INFO = "", -- TODO: find proper icon
    HINT = "", -- TODO: find proper icon
}
M.log_level = {
    TRACE = "✎", -- TODO: find proper icon
    DEBUG = "", -- TODO: find proper icon
    INFO = M.diagnostics.INFO,
    WARN = M.diagnostics.WARN,
    ERROR = M.diagnostics.ERROR,
}
M.todo = {
    TODO = M.ui.CheckBox.Check,
    FIXME = "\u{f188}", -- 
    NOTE = "\u{f249}",  -- 
    HACK = "\u{f490}",  -- 
    WARN = M.diagnostics.WARN,
    TEST = "\u{f0668}", -- 󰙨
    PERF = "\u{f0152}", -- 󰅒
    NOIMP = M.kind.Constructor,
}

M.ui.separator = {
    primary = {
        left = "\u{e0b0}",  -- 
        right = "\u{e0b2}", -- 
    },
    secondary = {
        left = "\u{e0b1}",  -- 
        right = "\u{e0b3}", -- 
    },
}
M.misc.copilot = {
    enabled = "\u{f4b8}",  -- 
    disabled = "\u{f4b9}", -- 
    sleep = "\u{f4b9}",    -- 
    warning = "\u{f4ba}",  -- 
    unknown = "\u{f05e}",  -- 
}

return M
