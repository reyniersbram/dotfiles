local keymap = require("core.keymap")
keymap("n", "<C-w>", ":Bdelete<CR>", { desc = "Close buffer" })
