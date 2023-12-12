local status_ok, bufferline = pcall(require, "bufferline")
if not status_ok then
vim.notify("Bufferline not found!")
return
end

local icons_status_ok, icons = pcall(require, "helpers.icons")
if not icons_status_ok then
vim.notify("icons not found")
end

bufferline.setup {
    options = {
        mode = "buffers",
        style_preset = bufferline.style_preset.default, -- see bufferline-style-preset
        themable = false,
        numbers = "ordinal", -- "none" | "ordinal" | "buffer_id" | "both" | function({ ordinal, id, lower, raise }): string,
        close_command = "Bdelete %d", -- can be a string | function, see "Mouse actions"
        right_mouse_command = "Bdelete %d", -- can be a string | function, see "Mouse actions"
        left_mouse_command = "buffer %d", -- can be a string | function, see "Mouse actions"
        middle_mouse_command = false, -- can be a string | function, see "Mouse actions"
        indicator = {
            -- NOTE: this plugin is designed with this icon in mind,
            -- and so changing this is NOT recommended, this is intended
            -- as an escape hatch for people who cannot bear it for whatever reason
            icon = '▎', -- this should be omitted if indicator style is not 'icon'
            style = "icon", -- "icon" | "underline" | "none"
        },
        buffer_close_icon = '󰅖',
        modified_icon = '●',
        close_icon = '',
        left_trunc_marker = '',
        right_trunc_marker = '',
        --- name_formatter can be used to change the buffer's label in the bufferline.
        --- Please note some names can/will break the
        --- bufferline so use this at your discretion knowing that it has
        --- some limitations that will *NOT* be fixed.
        -- name_formatter = function(buf)  -- buf contains:
                  -- name                | str        | the basename of the active file
                  -- path                | str        | the full path of the active file
                  -- bufnr (buffer only) | int        | the number of the active buffer
                  -- buffers (tabs only) | table(int) | the numbers of the buffers in the tab
                  -- tabnr (tabs only)   | int        | the "handle" of the tab, can be converted to its ordinal number using: `vim.api.nvim_tabpage_get_number(buf.tabnr)`
        --   -- e.g. remove extension from markdown files
        --     if buf.name:match('%.md') then
        --         return vim.fn.fnamemodify(buf.name, ':t:r')
        --     end
        -- end,
        max_name_length = 18,
        max_prefix_length = 15, -- prefix used when a buffer is de-duplicated
        truncate_names = true, -- whether or not tab names should be truncated
        tab_size = 18,
        diagnostics = "nvim_lsp",
        diagnostics_update_in_insert = false,
        --- The diagnostics indicator can be set to `false` to remove the indicators
        --- completely whilst still keeping the highlight of the buffer name.
        --- count is an integer representing total count of errors
        --- level is a string "error" | "warning"
        --- this should return a string
        --- Don't get too fancy as this function will be executed a lot
        diagnostics_indicator = function(count, level, diagnostics_dict, context)
            local icon = level:match("error") and icons.diagnostics.ERROR or
                level:match("warning") and icons.diagnostics.WARN or
                level:match("info") and icons.diagnostics.INFO or
                level:match("hint") and icons.diagnostics.HINT or
                icons.diagnostics.HINT
            return " " .. icon .. " " .. count
        end,
        -- NOTE: this will be called a lot so don't do any heavy processing here
        -- custom_filter = function(buf_number)
        --   -- filter out filetypes you don't want to see
        --   if vim.bo[buf_number].filetype ~= "<i-dont-want-to-see-this>" then
        --     return true
        --   end
        --   -- filter out by buffer name
        --   if vim.fn.bufname(buf_number) ~= "<buffer-name-I-dont-want>" then
        --     return true
        --   end
        --   -- filter out based on arbitrary rules
        --   -- e.g. filter out vim wiki buffer from tabline in your work repo
        --   if vim.fn.getcwd() == "<work-repo>" and vim.bo[buf_number].filetype ~= "wiki" then
        --     return true
        --   end
        -- end,
        offsets = {
            {
                filetype = "NvimTree",
                text = "NvimTree",
                text_align = "right",
                separator = false,
                padding = 0,
                highlight = "Keyword",
            }
        },
        color_icons = true,
        -- get_element_icon = function(element)
        --   -- element consists of {filetype: string, path: string, extension: string, directory: string}
        --   -- This can be used to change how bufferline fetches the icon
        --   -- for an element e.g. a buffer or a tab.
        --   -- e.g.
        --   local icon, hl = require('nvim-web-devicons').get_icon_by_filetype(element.filetype, { default = false })
        --   return icon, hl
        --   -- or
        --   local custom_map = {my_thing_ft: {icon = "my_thing_icon", hl}}
        --   return custom_map[element.filetype]
        -- end,
        show_buffer_icons = true,
        show_buffer_close_icons = false,
        show_close_icon = false,
        show_tab_indicators = true,
        show_duplicate_prefix = false,
        persist_buffer_sort = true, -- whether or not custom sorted buffers should persist
        separator_style = "slant",
        enforce_regular_tabs = true,
        always_show_bufferline = true,
        hover = {
            enabled = true,
            delay = 200,
            reveal = {"close"},
        },
        sort_by = 'insert_after_current' -- 'insert_after_current' |'insert_at_end' | 'id' | 'extension' | 'relative_directory' | 'directory' | 'tabs' | function(buffer_a, buffer_b)
        --     -- add custom logic
        --     return buffer_a.modified > buffer_b.modified
        -- end,
    },
    highlights = function ()
        local catppuccin_loaded, catppuccin_bufferline = pcall(require, "catppuccin.groups.integration.bufferline")
        if not catppuccin_loaded then
            return {}
        end
        return catppuccin_bufferline.get()
    end,
}
