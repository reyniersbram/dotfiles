local plugins = {
    "wbthomason/packer.nvim", -- packer plugin manager

    -- Colorschemes
    -- "ellisonleao/gruvbox.nvim",
    "navarasu/onedark.nvim",

    -- CMP
    "hrsh7th/nvim-cmp", -- The completion plugin
    -- sources
    "hrsh7th/cmp-buffer", -- buffer completions
    "hrsh7th/cmp-path", -- path completions
    "hrsh7th/cmp-cmdline", -- cmdline completions
    "saadparwaiz1/cmp_luasnip", -- snippet completions
    "hrsh7th/cmp-nvim-lsp", -- integration with lsp
    "hrsh7th/cmp-nvim-lua", -- extra nvim lua completion
 --    "hrsh7th/cmp-emoji", -- emojiiis🙂
 --    "f3fora/cmp-spell", -- spell checker
    -- snippets
    "L3MON4D3/LuaSnip", --snippet engine
    "rafamadriz/friendly-snippets", -- a bunch of snippets to use

    -- LSP
    "neovim/nvim-lspconfig",
    "williamboman/mason.nvim",
    -- "jose-elias-alvarez/null-ls.nvim",
    -- "ray-x/lsp_signature.nvim"
    -- "RRethy/vim-illuminate"
    -- "lvimuser/lsp-inlayhints.nvim"
    "fladson/vim-kitty",

    -- NVim Tree
    {
        "nvim-tree/nvim-tree.lua",
        requires = "nvim-tree/nvim-web-devicons",
    },

    -- ColorColumn
    "Bekaboo/deadcolumn.nvim",

    -- Bufferline
    {
        "akinsho/bufferline.nvim",
        requires = {
            "moll/vim-bbye",
            "nvim-tree/nvim-web-devicons",
        },
    },

    -- Social
    "andweeb/presence.nvim",
    "wakatime/vim-wakatime",






    -- Notifications
    "rcarriga/nvim-notify",

    -- Misc
    "nvim-lua/popup.nvim", -- An implementation of the Popup API from vim in Neovim
    "nvim-lua/plenary.nvim", -- Useful lua functions used by lots of plugins

    -- Markdown preview plugin
    {
        "iamcco/markdown-preview.nvim",
        run = "cd app && npm install",
        ft = "markdown",
    },

    -- Telescope
    {
        "nvim-telescope/telescope.nvim",
        tag = '0.1.0',
        requires = {
            "nvim-lua/plenary.nvim",
            {
               "nvim-telescope/telescope-fzf-native.nvim", -- Better telescope sorting
                run = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build"
            },
        },
    },
    -- "nvim-telescope/telescope-media-files.nvim",

    -- Color Highlighting
    "norcalli/nvim-colorizer.lua",

    -- Treesitter
    {
        "nvim-treesitter/nvim-treesitter",
        run = ":TSUpdate",
    },
    "JoosepAlviste/nvim-ts-context-commentstring",

    -- Autopairs
    "windwp/nvim-autopairs",

    -- Commenting
    "numToStr/Comment.nvim",

    -- Git
    "lewis6991/gitsigns.nvim",

    -- Lualine
    {
        "nvim-lualine/lualine.nvim",
        requires = {
            "nvim-tree/nvim-web-devicons",
            "arkav/lualine-lsp-progress",
        },
    },

    -- Nvim-Navic
    {
        "SmiteshP/nvim-navic",
        requires = {
            "neovim/nvim-lspconfig",
        },
    },
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

