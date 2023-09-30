local presence_status_ok, presence = pcall(require, "presence")
if not presence_status_ok then
    vim.notify("presence not found!")
    return
end

local ssh2http = require("helpers.utils").ssh2http

presence.setup {
    -- General options

    -- Update activity based on autocmd events (if `false`, map or manually
    -- execute `:lua package.loaded.presence:update()`)
    auto_update         = true,
    -- Text displayed when hovered over the Neovim image
    neovim_image_text   = "The One True Text Editor",
    -- Main image display (either "neovim" or "file")
    main_image          = "neovim",
    -- Use your own Discord application client id (not recommended)
    client_id           = "793271441293967371",
    -- Log messages at or above this level
    -- (one of the following: "debug", "info", "warn", "error")
    log_level           = nil,
    -- Number of seconds to debounce events (or calls to
    -- `:lua package.loaded.presence:update(<filename>, true)`)
    debounce_timeout    = 10,
    -- Displays the current line number instead of the current project
    enable_line_number  = false,
    -- A list of strings or Lua patterns that disable Rich Presence if the
    -- current file name, path, or workspace matches
    blacklist           = {},
    -- Configure Rich Presence button(s), either a boolean to enable/disable, a
    -- static table (`{{ label = "<label>", url = "<url>" }, ...}`, or a
    -- function(buffer: string, repo_url: string|nil): table)
    buttons             =
        function(buffer, repo_url)
            if repo_url == nil then
                return false
            end
            local excluded = {
                "term",
            }
            for _, item in pairs(excluded) do
                if string.match(repo_url, item) then
                    return false
                end
            end
            repo_url = ssh2http(repo_url)
            if repo_url == nil then
                return false
            end
            return {
                {
                    label = "View Repository",
                    url = repo_url,
                },
            }
        end,
    -- Custom file asset definitions keyed by file names and extensions (see
    -- default config at `lua/presence/file_assets.lua` for reference)
    file_assets         = {},
    -- Show the timer
    show_time           = true,

    -- Rich Presence text options
    editing_text        = "Editing %s",         -- Format string rendered when an editable file is loaded in the buffer (either string or function(filename: string): string)
    file_explorer_text  = "Browsing %s",        -- Format string rendered when browsing a file explorer (either string or function(file_explorer_name: string): string)
    git_commit_text     = "Committing changes", -- Format string rendered when committing changes in git (either string or function(filename: string): string)
    plugin_manager_text = "Managing plugins",   -- Format string rendered when managing plugins (either string or function(plugin_manager_name: string): string)
    reading_text        = "Reading %s",         -- Format string rendered when a read-only or unmodifiable file is loaded in the buffer (either string or function(filename: string): string)
    workspace_text      = "Working on %s",      -- Format string rendered when in a git repository (either string or function(project_name: string|nil, filename: string): string)
    line_number_text    = "Line %s out of %s",  -- Format string rendered when `enable_line_number` is set to true (either string or function(line_number: number, line_count: number): string)
}
