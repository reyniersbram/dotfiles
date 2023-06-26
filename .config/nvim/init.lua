vim.notify = require("notify")

require "core"
require "plugins"

-- Standard Configs
require "config.keymaps"
require "config.plugins"
require "config.colorscheme"

-- Coding Configs
require "config.treesitter"
require "config.autopairs"

-- Other Configs
require "config.gitsigns"
require "config.telescope"
require "config.comment"
require "config.bufferline"
require "config.toggleterm"
-- require "config.colorizer"

-- WIP
require "config.statusline"
