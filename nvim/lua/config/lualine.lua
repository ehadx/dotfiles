require("lualine").setup {
  options = {
    icons_enabled = true,
    theme = 'auto',
    component_separators = '|',
    section_separators = '|',
    disabled_filetypes = {
      statusline = {},
      winbar = {},
    },
    ignore_focus = {
    },
    always_divide_middle = true,
    always_show_tabline = false,
    globalstatus = false,
    refresh = {
      statusline = 100,
      tabline = 100,
      winbar = 100,
    }
  },
  sections = {
    lualine_a = {
      {
        'mode',
        fmt = function(str) return str:sub(1, 1) end
      }
    },
    lualine_b = {
      'branch',
      'diff',
      'diagnostics'
    },
    lualine_c = {
      {
        'filename',
        path = 1,
        newfile_status = true,
      },
    },
    lualine_x = {
      'filesize',
      'encoding',
      'fileformat',
      'filetype'
    },
    lualine_y = {
      'searchcount',
      'selectioncount',
      'progress'
    },
    lualine_z = {'location'}
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {
      {
        'filename',
        path = 0,
        newfile_status = true,
      },
    },
    lualine_x = {'location'},
    lualine_y = {},
    lualine_z = {}
  },
  tabline = {
    lualine_a = {
      {
        "tabs",
        tab_max_length = 60,
        use_mode_colors = true,
        mode = 1,
        path = 1
      }
    },
    lualine_b = {},
    lualine_c = {},
    lualine_x = {},
    lualine_y = {},
    -- lualine_z = {
    --   {
    --     "buffers",
    --     show_filename_only = true,
    --     mode = 3,
    --     use_mode_colors = true,
    --   }
    -- }
  },
  winbar = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {},
    lualine_x = {},
    lualine_y = {},
    lualine_z = {}
  },
  inactive_winbar = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {},
    lualine_x = {},
    lualine_y = {},
    lualine_z = {}
  },
  extensions = {
    "quickfix",
    "mason",
    "man"
  }
}
