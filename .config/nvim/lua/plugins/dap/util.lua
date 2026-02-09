local M = {}

---Get input from the user
---@param prompt string
---@param default string
---@param format? fun(choice: string): any
---@return fun(): thread
function M.get_input_callback(prompt, default, format)
    if format == nil then
        format = function(c)
            return c
        end
    end
    return function()
        return coroutine.create(function(dap_run)
            vim.ui.input({
                prompt = prompt,
                default = default,
            }, function(choice)
                coroutine.resume(dap_run, format(choice))
            end)
        end)
    end
end

---Get a list of user input seperating the entries
---@param prompt string The prompt shown
---@param sep? string The separator, defaults to any number of whitespace
---characters
---@return fun():thread
function M.get_input_list(prompt, sep)
    local function format(choice)
        return vim.split(choice or "", sep or "%s+", { trimempty = true })
    end
    return M.get_input_callback(prompt, "", format)
end

return M
