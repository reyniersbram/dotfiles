-- TODO: vim.lsp.inlay_hint api is still unstable
-- vim.api.nvim_create_autocmd("LspAttach", {
--     desc = "Setup inlay hints",
--     callback = function(event)
--         local id = vim.tbl_get(event, "data", "client_id")
--         local client = id and vim.lsp.get_client_by_id(id)
--         if client == nil or not client.supports_method("textDocument/inlayHints") then
--             return
--         end
--         vim.lsp.inlay_hint.enable(event.buf, true)
--     end
-- })
