local plugins = {
    "wbthomason/packer.nvim", -- Have packer manage itself
    "nvim-lua/popup.nvim", -- An implementation of the Popup API from vim in Neovim
    "nvim-lua/plenary.nvim", -- Useful lua functions used by lots of plugins
    -- "kyazdani42/nvim-web-devicons", -- Use web-devicons, used by some plugins

    -- Colorschemes
    "ellisonleao/gruvbox.nvim",
    --[[ {
        "monsonjeremy/onedark.nvim",
        branch = "treesitter",
    }, ]]
    "navarasu/onedark.nvim",

    -- Markdown preview plugin
    {
        "iamcco/markdown-preview.nvim",
        run = "cd app && npm install",
        ft = "markdown",
    },

    -- CMP plugins
	"hrsh7th/nvim-cmp", -- The completion plugin 
	"hrsh7th/cmp-buffer", -- buffer completions
	"hrsh7th/cmp-path", -- path completions
	"hrsh7th/cmp-cmdline", -- cmdline completions
	"saadparwaiz1/cmp_luasnip", -- snippet completions
	"hrsh7th/cmp-nvim-lsp", -- integration with lsp
	"hrsh7th/cmp-nvim-lua", -- extra lua completion
    "hrsh7th/cmp-emoji", -- emojiiis🙂
    "f3fora/cmp-spell", -- spell checker

    -- snippet plugins
    "L3MON4D3/LuaSnip", --snippet engine
    "rafamadriz/friendly-snippets", -- a bunch of snippets to use

    -- LSP
    "neovim/nvim-lspconfig",
--  "williamboman/nvim-lsp-installer", -- deprecated, use mason
    "williamboman/mason.nvim",
    "williamboman/mason-lspconfig.nvim",
--     "jose-elias-alvarez/null-ls.nvim",
--     "SmiteshP/nvim-navic"
--     "ray-x/lsp_signature.nvim"
--     "RRethy/vim-illuminate"
--     "lvimuser/lsp-inlayhints.nvim"

    -- Discord Rich Presence
    -- "andweeb/presence.nvim",

    -- Telescope
    {
        "nvim-telescope/telescope.nvim",
        tag = '0.1.0',
        requires = "nvim-lua/plenary.nvim"
    },
    -- "nvim-telescope/telescope-media-files.nvim",
    {
        "nvim-telescope/telescope-fzf-native.nvim", -- Better telescope sorting
        run = "make"
    },

    -- Color Highlighting
    "norcalli/nvim-colorizer.lua",

    -- Treesitter
    {
        "nvim-treesitter/nvim-treesitter",
        run = ":TSUpdate",
    },
    "p00f/nvim-ts-rainbow",
    "JoosepAlviste/nvim-ts-context-commentstring",

    -- Autopairs
    "windwp/nvim-autopairs",

    -- Commenting
    "numToStr/Comment.nvim",

    -- Git
    "lewis6991/gitsigns.nvim",

    -- NVim Tree
    {
        "nvim-tree/nvim-tree.lua",
        requires = "nvim-tree/nvim-web-devicons",
    },

    -- Bufferline
    {
        "akinsho/bufferline.nvim",
        requires = {
            "moll/vim-bbye",
            "nvim-tree/nvim-web-devicons",
        }
    },

    -- ToggleTerm
    "akinsho/toggleterm.nvim",

    -- Wakatime
    "wakatime/vim-wakatime",
}


local function ensure_packer()
    local install_path = vim.fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
    if vim.fn.empty(vim.fn.glob(install_path)) == 0 then
        return false
    end
    vim.notify("Installing packer")
    vim.fn.system({
        "git",
        "clone",
        "--depth",
        "1",
        "https://github.com/wbthomason/packer.nvim",
        install_path,
    })
    vim.cmd([[packadd packer.nvim]])
    return true
end

local function install_plugins(use)
    for _, plugin in ipairs(plugins) do
        use(plugin)
    end
end

-- Autocommand that runs `:PackerSync` whenever `plugins.lua` is updated
vim.cmd(
    [[
        augroup packer_user_config
            autocmd!
            autocmd BufWritePost plugins.lua source <afile> | PackerSync
        augroup end
    ]]
)

local packer_bootstrap = ensure_packer()

-- Use a protected call so we don't error out on first use
local packer_status_ok, packer = pcall(require, "packer")
if not packer_status_ok then
    return
end

local icons_status_ok, icons = pcall(require, "helpers.icons")
if not icons_status_ok then
    return
end

-- Have packer use a popup window
packer.init({
    display = {
        open_fn = function()
            return require("packer.util").float({ border = "rounded" })
        end,
        working_sym = icons.ui.status.Loading, -- The symbol for a plugin being installed/updated
        error_sym = icons.ui.status.Failed, -- The symbol for a plugin with an error in installation/updating
        done_sym = icons.ui.status.Done, -- The symbol for a plugin which has completed installation/updating
        removed_sym = icons.git.Remove, -- The symbol for an unused plugin which was removed
        moved_sym = icons.documents.SymLinkArrow, -- The symbol for a plugin which was moved (e.g. from opt to start)
        header_sym = '━', -- The symbol for the header line in packer's display
    },
})

return packer.startup(
    function(use)
        install_plugins(use)
        if packer_bootstrap then packer.sync() end
    end
)

