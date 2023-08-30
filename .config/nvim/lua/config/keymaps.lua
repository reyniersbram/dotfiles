local opts = { noremap = true, silent = true }

-- Shorten function name
local keymap = vim.api.nvim_set_keymap

-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",

-- Toggle NvimTree
keymap("n", "<leader>e", ":NvimTreeToggle<CR>", opts)

-- Buffer Navigation
-- requires akinsho/bufferline.nvim
keymap("n", "<A-h>", ":BufferLineMovePrev<CR>", opts)
keymap("n", "<A-j>", ":BufferLineCyclePrev<CR>", opts)
keymap("n", "<A-k>", ":BufferLineCycleNext<CR>", opts)
keymap("n", "<A-l>", ":BufferLineMoveNext<CR>", opts)
keymap("n", "<A-1>", ":BufferLineGoToBuffer 1<CR>", opts)
keymap("n", "<A-2>", ":BufferLineGoToBuffer 2<CR>", opts)
keymap("n", "<A-3>", ":BufferLineGoToBuffer 3<CR>", opts)
keymap("n", "<A-4>", ":BufferLineGoToBuffer 4<CR>", opts)
keymap("n", "<A-5>", ":BufferLineGoToBuffer 5<CR>", opts)
keymap("n", "<A-6>", ":BufferLineGoToBuffer 6<CR>", opts)
keymap("n", "<A-7>", ":BufferLineGoToBuffer 7<CR>", opts)
keymap("n", "<A-8>", ":BufferLineGoToBuffer 8<CR>", opts)
keymap("n", "<A-9>", ":BufferLineGoToBuffer 9<CR>", opts)
keymap("n", "<A-0>", ":BufferLineGoToBuffer -1<CR>", opts)
keymap("n", "<A-w>", ":BufferLineCloseOthers<CR>", opts)
-- requires moll/vim-bbye
keymap("n", "<C-w>", ":Bdelete<CR>", opts)

-- Toggle Telescope
keymap("n", "<leader>ff", ":Telescope find_files hidden=true<CR>", opts)

