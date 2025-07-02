return {
    cmd = { "lua-language-server" },
    root_markers = {
        ".luarc.json", ".luarc.jsonc",
        ".luacheckrc",
        ".stylua.toml", "stylua.toml",
        "selene.toml", "selene.yml",
        ".git"
    },
    filetypes = { "lua" },
    settings = {
        Lua = {
            hint = {
                enable = true,
                arrayIndex = "Disable",
                await = true,
                awaitPropagate = true,
                paramName = "All",
                paramType = true,
                semicolon = "Disable",
                setType = true,
            },
        },
    },
}
