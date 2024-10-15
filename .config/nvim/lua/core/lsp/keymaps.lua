local keymap = require("core.keymap")

local M = {}

local telescope = require("telescope.builtin")

---@param client vim.lsp.Client
---@param bufnr integer
local function set_keymaps(client, bufnr)
    local opts = { buffer = bufnr }
    if client.supports_method("textDocument/codeAction") then
        keymap({ "n", "v" }, "ga", vim.lsp.buf.code_action, opts)
    end
    if client.supports_method("textDocument/formating") then
        keymap({ "n", "v" }, "gf", function()
            vim.lsp.buf.format {
                -- filter = function(_client) return _client.name ~= "ts_ls" end
            }
        end, opts)
    end
    if client.supports_method("textDocument/hover") then
        keymap("n", "K", vim.lsp.buf.hover, opts)
    end
    if client.supports_method("textDocument/rename") then
        keymap("n", "<leader>r", vim.lsp.buf.rename, opts)
    end
    if client.supports_method("textdocument/signatureHelp") then
        keymap("i", "<C-s>", vim.lsp.buf.signature_help, opts)
    end

    -- TODO: loc-/qflist? https://neovim.io/doc/user/lsp.html#vim.lsp.ListOpts
    keymap("n", "gd",
        function()
            vim.lsp.buf.definition { reuse_win = true }
        end, opts)
    keymap("n", "gD",
        function()
            vim.lsp.buf.declaration { reuse_win = true }
        end, opts)
    keymap("n", "gi",
        function()
            vim.lsp.buf.implementation { reuse_win = true }
        end, opts)
    keymap("n", "gR",
        function()
            vim.lsp.buf.references { reuse_win = true }
        end, opts)
    keymap("n", "gt", vim.lsp.buf.type_definition, opts)
    keymap("n", "gT", vim.lsp.buf.typehierarchy, opts)



    -- Using Telescope
    -- vim.api.nvim_buf_set_keymap(bufnr, "n", "gD", "<cmd>Telescope lsp_declarations<<CR>", opts)
    -- vim.api.nvim_buf_set_keymap(bufnr, "n", "gd", "<cmd>Telescope lsp_definitions<CR>", opts)
    -- vim.api.nvim_buf_set_keymap(bufnr, "n", "gi", "<cmd>Telescope lsp_implementations<CR>", opts)

    -- TODO check if I want these
    -- vim.api.nvim_buf_set_keymap(bufnr, "n", "gr", "<cmd>Telescope lsp_references<CR>", opts)
    -- vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>q", "<cmd>lua vim.diagnostic.setloclist()<CR>", opts)
end

---@param client vim.lsp.Client
---@param bufnr integer
function M.on_attach(client, bufnr)
    set_keymaps(client, bufnr)
end

function M.setup()
end

return M
