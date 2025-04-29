local map = require("core.keymap")

map("n", "<localleader><localleader>x", ":source %<CR>", { buffer = true, desc = "Source current lua file" })
map("v", "<localleader>x", ":lua<CR>", { buffer = true, desc = "Execute current selection of lua code" })
map("n", "<localleader>x", ":.lua<CR>", { buffer = true, desc = "Execute current line of lua code" })
