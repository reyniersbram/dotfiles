local status_ok, alpha = pcall(require, "alpha")
if not status_ok then
    vim.notify("Alpha not found")
end
alpha.setup(require("alpha.themes.theta").config)
-- alpha.setup {
--     layout = {
--         {
--             type = "text",
--             val = "some text",
--         },
--         {
--             type = "padding",
--             val = 1,
--         },
--         {
--             type = "button",
--             val = "some button",
--             on_press = function()
--                 vim.notify("hey")
--             end,
--             opts = {
--                 -- position = "center",
--                 -- shortcut = "e",
--                 keymap = {
--                     "n",
--                     "b",
--                     function() vim.notify("hey") end,
--                     { noremap = true, silent = true, nowait = true }
--                 }
--             },
--         },
--     },
-- }
