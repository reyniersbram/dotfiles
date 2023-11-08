local function on_attach(bufnr)
    local api_status_ok, api = pcall(require, "nvim-tree.api")
    if not api_status_ok then
        return
    end

    local function opts(desc)
        return {
            desc = 'nvim-tree: ' .. desc,
            buffer = bufnr,
            noremap = true,
            silent = true,
            nowait = true,
        }
    end

    vim.keymap.set('n', '<2-LeftMouse>',  api.node.open.edit,           opts('Open'))
    vim.keymap.set('n', '<2-RightMouse>', api.tree.change_root_to_node, opts('CD'))
    vim.keymap.set('n', '<C-]>', api.tree.change_root_to_node,          opts('CD'))
    vim.keymap.set('n', '<C-e>', api.node.open.replace_tree_buffer,     opts('Open: In Place'))
    vim.keymap.set('n', '<C-k>', api.node.show_info_popup,              opts('Info'))
    vim.keymap.set('n', '<C-r>', api.fs.rename_sub,                     opts('Rename: Omit Filename'))
    vim.keymap.set('n', '<C-t>', api.node.open.tab,                     opts('Open: New Tab'))
    vim.keymap.set('n', '<BS>',  api.node.navigate.parent_close,        opts('Close Directory'))
    vim.keymap.set('n', '<CR>',  api.node.open.edit,                    opts('Open'))
    vim.keymap.set('n', '<Tab>', api.node.open.preview,                 opts('Open Preview'))
    vim.keymap.set('n', '>',     api.node.navigate.sibling.next,        opts('Next Sibling'))
    vim.keymap.set('n', '<',     api.node.navigate.sibling.prev,        opts('Previous Sibling'))
    vim.keymap.set('n', '.',     api.node.run.cmd,                      opts('Run Command'))
    vim.keymap.set('n', '-',     api.tree.change_root_to_parent,        opts('Up'))
    vim.keymap.set('n', 'B',     api.tree.toggle_no_buffer_filter,      opts('Toggle No Buffer'))
    vim.keymap.set('n', 'C',     api.tree.toggle_git_clean_filter,      opts('Toggle Git Clean'))
    vim.keymap.set('n', 'D',     api.fs.trash,                          opts('Trash'))
    vim.keymap.set('n', 'E',     api.tree.expand_all,                   opts('Expand All'))
    vim.keymap.set('n', 'F',     api.live_filter.clear,                 opts('Clean Filter'))
    vim.keymap.set('n', 'H',     api.tree.toggle_hidden_filter,         opts('Toggle Dotfiles'))
    vim.keymap.set('n', 'I',     api.tree.toggle_gitignore_filter,      opts('Toggle Git Ignore'))
    vim.keymap.set('n', 'J',     api.node.navigate.sibling.last,        opts('Last Sibling'))
    vim.keymap.set('n', 'K',     api.node.navigate.sibling.first,       opts('First Sibling'))
    vim.keymap.set('n', 'O',     api.node.open.no_window_picker,        opts('Open: No Window Picker'))
    vim.keymap.set('n', 'P',     api.node.navigate.parent,              opts('Parent Directory'))
    vim.keymap.set('n', 'R',     api.tree.reload,                       opts('Refresh'))
    vim.keymap.set('n', 'S',     api.tree.search_node,                  opts('Search'))
    vim.keymap.set('n', 'U',     api.tree.toggle_custom_filter,         opts('Toggle Hidden'))
    vim.keymap.set('n', 'W',     api.tree.collapse_all,                 opts('Collapse'))
    vim.keymap.set('n', 'Y',     api.fs.copy.relative_path,             opts('Copy Relative Path'))
    vim.keymap.set('n', 'a',     api.fs.create,                         opts('Create'))
    vim.keymap.set('n', 'bmv',   api.marks.bulk.move,                   opts('Move Bookmarked'))
    vim.keymap.set('n', 'bd',    api.marks.bulk.delete,                 opts('Delete Bookmarked'))
    vim.keymap.set('n', 'c',     api.fs.copy.node,                      opts('Copy'))
    vim.keymap.set('n', '[c',    api.node.navigate.git.prev,            opts('Prev Git'))
    vim.keymap.set('n', ']c',    api.node.navigate.git.next,            opts('Next Git'))
    vim.keymap.set('n', 'd',     api.fs.remove,                         opts('Delete'))
    vim.keymap.set('n', 'e',     api.fs.rename_basename,                opts('Rename: Basename'))
    vim.keymap.set('n', ']e',    api.node.navigate.diagnostics.next,    opts('Next Diagnostic'))
    vim.keymap.set('n', '[e',    api.node.navigate.diagnostics.prev,    opts('Prev Diagnostic'))
    vim.keymap.set('n', 'f',     api.live_filter.start,                 opts('Filter'))
    vim.keymap.set('n', 'g?',    api.tree.toggle_help,                  opts('Help'))
    vim.keymap.set('n', 'gy',    api.fs.copy.absolute_path,             opts('Copy Absolute Path'))
    vim.keymap.set('n', 'h',     api.node.open.horizontal,              opts('Open: Horizontal Split'))
    vim.keymap.set('n', 'l',     api.node.open.edit,                    opts('Open'))
    vim.keymap.set('n', 'm',     api.marks.toggle,                      opts('Toggle Bookmark'))
    vim.keymap.set('n', 'o',     api.node.open.edit,                    opts('Open'))
    vim.keymap.set('n', 'p',     api.fs.paste,                          opts('Paste'))
    vim.keymap.set('n', 'q',     api.tree.close,                        opts('Close'))
    vim.keymap.set('n', 'r',     api.fs.rename,                         opts('Rename'))
    vim.keymap.set('n', 's',     api.node.run.system,                   opts('Run System'))
    vim.keymap.set('n', 'v',     api.node.open.vertical,                opts('Open: Vertical Split'))
    vim.keymap.set('n', 'x',     api.fs.cut,                            opts('Cut'))
    vim.keymap.set('n', 'y',     api.fs.copy.filename,                  opts('Copy Name'))
end

local nvim_tree_status_ok, nvim_tree = pcall(require, "nvim-tree")
if not nvim_tree_status_ok then
    vim.notify("nvim-tree not found")
    return
end

local icons_status_ok, icons = pcall(require, "helpers.icons")
if not icons_status_ok then
    vim.notify("icons not found")
    return
end

nvim_tree.setup {
    disable_netrw = true,
    hijack_netrw = true,
    auto_reload_on_write = true,
    sort_by = "name",
    hijack_unnamed_buffer_when_opening = true,
    hijack_cursor = true,
    root_dirs = {},
    prefer_startup_root = false,
    sync_root_with_cwd = false,
    reload_on_bufenter = false,
    respect_buf_cwd = false,
    hijack_directories = {
        enable = true,
        auto_open = true,
    },
    update_focused_file = {
        enable = true,
        update_root = true,
        ignore_list = {},
    },
    system_open = {
        cmd = "",
        args = {},
    },
    diagnostics = {
        enable = true,
        debounce_delay = 50,
        show_on_dirs = true,
        show_on_open_dirs = false,
        icons = {
            hint = icons.diagnostics.Hint,
            info = icons.diagnostics.Information,
            warning = icons.diagnostics.Warning,
            error = icons.diagnostics.Error,
        },
        severity = {
            min = vim.diagnostic.severity.HINT,
            max = vim.diagnostic.severity.ERROR,
        },
    },
    git = {
        enable = true,
        ignore = false,
        show_on_dirs = true,
        show_on_open_dirs = false,
        timeout = 400,
    },
    modified = {
        enable = true,
        show_on_dirs = true,
        show_on_open_dirs = false,
    },
    filesystem_watchers = {
        enable = true,
        debounce_delay = 50,
        ignore_dirs = {}, -- TODO
    },
    on_attach = on_attach,
    select_prompts = false, -- TODO
    view = {
        centralize_selection = false,
        cursorline = true,
        debounce_delay = 15,
        width = {
            min = 30,
            max = 30,
            padding = 1,
        },
        side = "left",
        preserve_window_proportions = true,
        number = true,
        relativenumber = true,
        signcolumn = "yes",
        float = {
            enable = false,
            -- quit_on_focus_loss = true,
            -- open_win_config = {
            --     relative = "editor",
            --     border = "rounded",
            --     width = 30,
            --     height = 30,
            --     row = 1,
            --     col = 1,
            -- },
        },
    },
    renderer = {
        add_trailing = false,
        group_empty = false,
        full_name = true,
        highlight_git = true,
        highlight_opened_files = "none",
        highlight_modified = "none",
        root_folder_label = ":~",
        indent_width = 2,
        indent_markers = {
            enable = true,
            inline_arrows = true,
            icons = {
                corner = icons.documents.Indent.Corner,
                edge = icons.documents.Indent.Edge,
                item = icons.documents.Indent.Item,
                bottom = icons.documents.Indent.Bottom,
                none = icons.documents.Indent.None,
            }
        },
        icons = {
            webdev_colors = true,
            git_placement = "before",
            modified_placement = "after",
            padding = " ",
            symlink_arrow = " " .. icons.documents.SymLinkArrow .. " ",
            show = {
                file = true,
                folder = true,
                folder_arrow = true,
                git = true,
                modified = true,
            },
            glyphs = {
                default = icons.documents.File,
                symlink = icons.documents.SymLink,
                bookmark = icons.ui.BookMark,
                modified = icons.ui.Circle,
                folder = {
                    arrow_closed = icons.documents.Folder.Collapsed,
                    arrow_open = icons.documents.Folder.Expanded,
                    default = icons.documents.Folder.Folder,
                    open = icons.documents.Folder.Open,
                    empty = icons.documents.Folder.Empty,
                    empty_open = icons.documents.Folder.EmptyOpen,
                    symlink = icons.documents.Folder.SymLink,
                    symlink_open = icons.documents.Folder.SymLink,
                },
                git = {
                    unstaged = icons.git.Mod,
                    staged = icons.git.Commit,
                    unmerged = icons.git.Branch,
                    renamed = icons.git.Rename,
                    untracked = icons.git.Add,
                    deleted = icons.git.Remove,
                    ignored = icons.git.Ignore,
                },
            },
        },
        special_files = { "Cargo.toml", "Makefile", "README.md", "readme.md", "LICENSE" },
        symlink_destination = true,
    },
    filters = {
        dotfiles = false,
        git_clean = false,
        no_buffer = false,
        custom = {},
        exclude = {},
    },
    trash = {
        cmd = "gio trash",
    },
    actions = {
        change_dir = {
            enable = true,
            global = false,
            restrict_above_cwd = true,
        },
        expand_all = {
            max_folder_discovery = 300,
            exclude = { ".git", "build", "node_modules", "__pycache__" },
        },
        file_popup = {
            open_win_config = {
                col = 1,
                row = 1,
                relative = "cursor",
                border = "shadow",
                style = "minimal",
            },
        },
        open_file = {
            quit_on_open = false,
            resize_window = true,
            window_picker = {
                enable = true,
                picker = "default",
                chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890",
                exclude = {
                    filetype = { "notify", "packer", "qf", "diff", "fugitive", "fugitiveblame" },
                    buftype = { "nofile", "terminal", "help" },
                },
            },
        },
        remove_file = {
            close_window = true,
        },
        use_system_clipboard = true,
    },
    live_filter = {
        prefix = "[FILTER]: ",
        always_show_folders = false,
    },
    tab = {
        sync = {
            open = true,
            close = true,
            ignore = {},
        },
    },
    notify = {
        threshold = vim.log.levels.INFO,
    },
    ui = {
        confirm = {
            remove = true,
            trash = true,
        },
    },
    experimental = {
        git = {
            async = true,
        },
    },
    log = {
        enable = false,
        truncate = false,
        types = {
            all = false,
            config = false,
            copy_paste = false,
            dev = false,
            diagnostics = false,
            git = false,
            profile = false,
            watcher = false,
        },
    },
}
