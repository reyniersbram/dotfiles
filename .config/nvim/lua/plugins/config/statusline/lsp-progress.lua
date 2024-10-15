local lsp_progress = require("lsp-progress")

--- Verify if the client is valid
--- @param client table
--- @return boolean
local function verify_client(client)
    return type(client) == "table"
        and type(client.name) == "string"
        and string.len(client.name) > 0
        and client.name ~= "copilot"
end

--- Stringify the client name and message
--- @param name string
--- @param message string?
--- @return string
local function stringify(name, message)
    return message and name .. " " .. message or name
end

lsp_progress.setup {
    spinner = require("util.icons").ui.spinner,
    spin_update_time = 125,
    decay = 700,
    event = "LspProgressUpdate",
    event_update_time_limit = 100,
    max_size = -1,
    regular_internal_update_time = 500,
    disable_events_opts = {
    },
    series_format = function(title, message, percentage, done)
        local builder = {}
        local has_title = false
        local has_message = false
        if type(title) == "string" and title ~= "" then
            has_title = true
            table.insert(builder, title)
        end
        if type(message) == "string" and message ~= "" then
            has_message = true
            table.insert(builder, message)
        end
        if percentage and (has_title or has_message) then
            table.insert(builder, string.format("%.0f%%", percentage))
        end
        if done and (has_title or has_message) then
            table.insert(builder, "- done")
        end
        return table.concat(builder, " ")
    end,
    client_format = function(client_name, spinner, series_messages)
        if #series_messages <= 0 then
            return nil
        end
        return {
            name = client_name,
            body = spinner .. " " .. table.concat(series_messages, " "),
        }
    end,
    format = function(client_messages)
        -- TODO: fix icon
        local sign = " LSP"
        local lsp_clients = vim.lsp.get_active_clients()
        local messages = {}

        for _, client_message in ipairs(client_messages) do
            messages[client_message.name] = client_message.body
        end

        if #lsp_clients > 1 then
            table.sort(lsp_clients, function(a, b)
                return a.name < b.name
            end)
            local builder = {}
            for _, client in ipairs(lsp_clients) do
                if verify_client(client) then
                    if messages[client.name] then
                        table.insert(builder, stringify(client.name, messages[client.name]))
                    else
                        table.insert(builder, stringify(client.name))
                    end
                end
            end
            if #builder > 0 then
                return sign .. " " .. table.concat(builder, ", ")
            end
        end
        return ""
    end,
    debug = false,
    console_log = false,
    file_log = false,
    file_log_name = "lsp_progress.log",
}
