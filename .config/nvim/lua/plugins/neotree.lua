return {
  "nvim-neo-tree/neo-tree.nvim",
  branch = "v3.x",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
    "MunifTanjim/nui.nvim",
    "3rd/image.nvim", -- Optional image support in preview window: See `# Preview Mode` for more information
  },
  config = function()
    local map_opts = { noremap = true, silent = true }
    vim.keymap.set('n', '<leader>e', ':Neotree toggle<CR>', map_opts)

    require("neo-tree").setup({
      sources = {
        "filesystem",
        "buffers",
        "git_status",
        "document_symbols",
      },
      source_selector = {
        winbar = true,
        statusline = false
      },
      event_handlers = {
        {
          event = "neo_tree_window_after_open",
          handler = function()
            vim.opt.laststatus = 3
          end
        }
      },
      window = {
        position = "left",
        width = 30,
      },
      filesystem = {
        filtered_items = {
          visible = false, -- when true, they will just be displayed differently than normal items
          hide_dotfiles = false,
          hide_gitignored = false,
          hide_hidden = false, -- only works on Windows for hidden files/directories
          hide_by_name = {
            --"node_modules"
          },
          hide_by_pattern = { -- uses glob style patterns
            --"*.meta",
            --"*/src/*/tsconfig.json",
          },
          always_show = { -- remains visible even if other settings would normally hide it
            --".gitignored",
          },
          always_show_by_pattern = { -- uses glob style patterns
            --".env*",
          },
          never_show = { -- remains hidden even if visible is toggled to true, this overrides always_show
            --".DS_Store",
            --"thumbs.db"
          },
          never_show_by_pattern = { -- uses glob style patterns
            --".null-ls_*",
          },
        },
        follow_current_file = {
          enabled = false, -- This will find and focus the file in the active buffer every time
          --               -- the current file is changed while the tree is open.
          leave_dirs_open = true, -- `false` closes auto expanded dirs, such as with `:Neotree reveal`
        },
        group_empty_dirs = false, -- when true, empty folders will be grouped together
        hijack_netrw_behavior =  "open_current",
      }
    })
  end 
}
