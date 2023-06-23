local prequire = require("helpers.utils").prequire

local M = {}

-- Capabilities
M.capabilities = vim.lsp.protocol.make_client_capabilities()
M.capabilities.textDocument.completion.completionItem.snippetSupport = true

-- CMP integration
local cmp_nvim_lsp_status_ok, cmp_nvim_lsp = prequire("cmp_nvim_lsp")
if not cmp_nvim_lsp_status_ok then
    vim.notify("cmp_nvim_lsp not found")
    return
end
M.capabilities = cmp_nvim_lsp.default_capabilities(M.capabilities)

-- On Attach
local function set_keymaps(bufnr)
    local opts = { noremap = true, silent = true, buffer = bufnr }

    vim.keymap.set("n", "gl", vim.diagnostic.open_float, opts)
    vim.keymap.set("n", "ge", vim.diagnostic.goto_next, opts)
    vim.keymap.set("n", "gE", vim.diagnostic.goto_prev, opts)

    vim.keymap.set("n", "<leader>r", vim.lsp.buf.rename, opts)
    vim.keymap.set("n", "gd",
        function()
            vim.lsp.buf.definition { reuse_win = true }
        end, opts)
    vim.keymap.set("n", "gD",
        function()
            vim.lsp.buf.declaration { reuse_win = true }
        end, opts)
    vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
    vim.keymap.set("n", "gi", vim.lsp.buf.implementation, opts)
    vim.keymap.set("n", "gs", vim.lsp.buf.signature_help, opts)

    vim.keymap.set("n", "gf", vim.lsp.buf.format, opts)
    vim.keymap.set("n", "ga", vim.lsp.buf.code_action, opts)

    -- Using Telescope
    -- vim.api.nvim_buf_set_keymap(bufnr, "n", "gD", "<cmd>Telescope lsp_declarations<<CR>", opts)
    -- vim.api.nvim_buf_set_keymap(bufnr, "n", "gd", "<cmd>Telescope lsp_definitions<CR>", opts)
    -- vim.api.nvim_buf_set_keymap(bufnr, "n", "gi", "<cmd>Telescope lsp_implementations<CR>", opts)

    -- TODO check if I want these
    -- vim.api.nvim_buf_set_keymap(bufnr, "n", "gr", "<cmd>Telescope lsp_references<CR>", opts)
    -- vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>q", "<cmd>lua vim.diagnostic.setloclist()<CR>", opts)
    -- vim.cmd([[ command! Format execute 'lua vim.lsp.buf.format()' ]])
end

M.on_attach = function(client, bufnr)
    set_keymaps(bufnr)
    -- lsp_highlight_document(client)
end


-- TODO uitzoeken wat dit doet
local function lsp_highlight_document(client)
    -- Set autocommands conditional on server_capabilities
    if client.resolved_capabilities.document_highlight then
        vim.api.nvim_exec(
            [[
                augroup lsp_document_highlight
                autocmd! * <buffer>
                autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
                autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
                augroup END
            ]],
            false
        )
    end
end

-- TODO uitzoeken wat dit doet
-- function M.enable_format_on_save()
--     vim.cmd(
--         [[ augroup format_on_save
--             autocmd!
--             autocmd BufWritePre * lua vim.lsp.buf.format({ async = false })
--         augroup END ]]
--     )
--     vim.notify "Enabled format on save"
-- end
--
-- function M.disable_format_on_save()
--     M.remove_augroup "format_on_save"
--     vim.notify "Disabled format on save"
-- end
--
-- function M.toggle_format_on_save()
--     if vim.fn.exists "#format_on_save#BufWritePre" == 0 then
--         M.enable_format_on_save()
--     else
--         M.disable_format_on_save()
--     end
-- end
--
-- function M.remove_augroup(name)
--     if vim.fn.exists("#" .. name) == 1 then
--         vim.cmd("au! " .. name)
--     end
-- end

-- vim.cmd [[ command! LspToggleAutoFormat execute 'lua require("user.lsp.handlers").toggle_format_on_save()' ]]

return M
