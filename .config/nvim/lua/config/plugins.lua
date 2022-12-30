local plugins = {
    "wbthomason/packer.nvim", -- Have packer manage itself
    "nvim-lua/popup.nvim", -- An implementation of the Popup API from vim in Neovim
    "nvim-lua/plenary.nvim", -- Useful lua functions used by lots of plugins
    "kyazdani42/nvim-web-devicons", -- Use web-devicons, used by some plugins

    -- Colorschemes
    "ellisonleao/gruvbox.nvim",

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
	"hrsh7th/cmp-emoji",
    --[[ "f3fora/cmp-spell", ]]

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
    "andweeb/presence.nvim",

    -- Telescope
    "nvim-telescope/telescope.nvim",
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
    "kyazdani42/nvim-tree.lua",

    -- Bufferline
    "akinsho/bufferline.nvim",
    "moll/vim-bbye",

    -- ToggleTerm
    "akinsho/toggleterm.nvim",

}


local fn = vim.fn

-- Automatically install packer
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
    PACKER_BOOTSTRAP = fn.system({
        "git",
        "clone",
        "--depth",
        "1",
        "https://github.com/wbthomason/packer.nvim",
        install_path,
    })
    print("Installing packer close and reopen Neovim...")
    vim.cmd([[packadd packer.nvim]])
end

-- Autocommand that reloads neovim whenever you save the plugins.lua file
vim.cmd(
    [[
        augroup packer_user_config
        autocmd!
        autocmd BufWritePost plugins.lua source <afile> | PackerSync
        augroup end
    ]]
)

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
    return
end
-- Have packer use a popup window
packer.init({
    display = {
        open_fn = function()
            return require("packer.util").float({ border = "rounded" })
        end,
    },
})

-- Install your plugins here
return packer.startup(function(use)
    for _, plugin in ipairs(plugins) do
        use(plugin)
    end

    -- Automatically set up your configuration after cloning packer.nvim
    -- Put this at the end after all plugins
    if PACKER_BOOTSTRAP then
        require("packer").sync()
    end
end)

