local M = {}

--- Manage a floating terminal window
--- When opening the window, it will enter terminal mode
--- Available commands:
--- FloatTermOpen:   open the window
--- FloatTermClose:  close the window
--- FloatTermToggle: toggle the window
---
--- Available <Plug> keymaps:
--- <Plug>FloatTermOpen:     open the window
--- <Plug>FloatTermClose:    close the window
--- <Plug>FloatTermToggle:   toggle the window
---
--- Available lua functions:
--- toggle_float_term(force_open?: boolean)

---@type vim.api.keyset.win_config
local win_opts = {
    relative = "editor",
    row = 3,
    col = 10,
    width = 200,
    height = 50,
}

---@class FloatingWindow
---@field buf integer
---@field win integer

---@class VimState : FloatingWindow
local vim_state = {
    win = -1,
    buf = -1,
}
function vim_state:set(new_value)
    for key, value in pairs(new_value) do
        vim_state[key] = value
    end
end

--- Open a new floating window. Tries to recover the buffer from a previous
--- floating window.
---@param opts FloatingWindow The previous floating window
---@return FloatingWindow # The new floating window
local function open_float_win(opts)
    -- Calculate size and position
    win_opts.width = math.floor(vim.opt.columns:get() * 0.5)
    win_opts.height = math.floor(vim.opt.lines:get() * 0.5)
    win_opts.col = math.floor((vim.opt.columns:get() - win_opts.width) / 2)
    win_opts.row = math.floor((vim.opt.lines:get() - win_opts.height) / 2)

    -- Recover old buffer, otherwise create a new one
    local bufnr = nil
    if vim.api.nvim_buf_is_valid(opts.buf) then
        bufnr = opts.buf
    else
        bufnr = vim.api.nvim_create_buf(false, true)
        if bufnr == 0 then
            error("Could not create new terminal buffer", 1)
        end
    end
    -- Open new window with the buffer
    local winnr = vim.api.nvim_open_win(bufnr, true, win_opts)
    if winnr == 0 then
        error("Could not create new terminal window", 1)
    end

    return { buf = bufnr, win = winnr }
end

--- Close a floating window
---@param opts FloatingWindow
local function close_float_win(opts)
    vim.api.nvim_win_hide(opts.win)
end

--- Toggle a floating terminal. Opening a terminal will restore the state of the
--- terminal before closing it.
---@param force_open? boolean Force open/close the floating terminal
function M.toggle_float_term(force_open)
    -- Force close the terminal
    if force_open == false then
        close_float_win(vim_state)
        return
    end
    -- Open a new terminal if none exists yet, else close it
    if not vim.api.nvim_win_is_valid(vim_state.win) then
        -- Try to open a new window
        local open_ok, new_state = pcall(open_float_win, vim_state)
        if not open_ok then
            vim.notify("Could not open terminal", vim.log.levels.ERROR)
            return
        end
        -- Update the window state
        vim_state:set(new_state)
        -- Set the buffer to be a terminal
        if vim.opt_local.buftype:get() ~= "terminal" then
            vim.cmd.terminal()
        end
        -- The terminal should not show up in the buffer list
        vim.opt_local.buflisted = false
        -- Start the terminal in insert mode
        vim.cmd("startinsert")
    else
        close_float_win(vim_state)
    end
end

-- User commands
vim.api.nvim_create_user_command("FloatTermOpen",
    function() M.toggle_float_term(true) end,
    { desc = "Open a reusable floating terminal" })
vim.api.nvim_create_user_command("FloatTermClose",
    function() M.toggle_float_term(false) end,
    { desc = "Close a reusable floating terminal" })
vim.api.nvim_create_user_command("FloatTermToggle", M.toggle_float_term,
    { desc = "Toggle a reusable floating terminal" })

-- Keymaps
vim.keymap.set({ "", "!", "t" }, "<Plug>FloatTermOpen",
    function() M.toggle_float_term(true) end,
    { desc = "Open a reusable floating terminal" })
vim.keymap.set({ "", "!", "t" }, "<Plug>FloatTermClose",
    function() M.toggle_float_term(false) end,
    { desc = "Close a reusable floating terminal" })
vim.keymap.set({ "", "!", "t" }, "<Plug>FloatTermToggle", M.toggle_float_term,
    { desc = "Toggle a reusable floating terminal" })

return M
