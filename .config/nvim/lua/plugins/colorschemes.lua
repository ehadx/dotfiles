return {
  {
    "scottmckendry/cyberdream.nvim",
    lazy = false,
    priority = 1000,
    opts = {
      -- Enable transparent background
      transparent = false,
      -- Enable italics comments
      italic_comments = true,
      -- Replace all fillchars with ' ' for the ultimate clean look
      hide_fillchars = false,
      -- Modern borderless telescope theme - also applies to fzf-lua
      borderless_telescope = false,
      -- Set terminal colors used in `:terminal`
      terminal_colors = true,
      -- Use caching to improve performance - WARNING: experimental feature - expect the unexpected!
      -- Early testing shows a 60-70% improvement in startup time. YMMV. Disables dynamic light/dark theme switching.
      cache = false, -- generate cache with :CyberdreamBuildCache and clear with :CyberdreamClearCache
      theme = {
          variant = "default", -- use "light" for the light variant. Also accepts "auto" to set dark or light colors based on the current value of `vim.o.background`
          highlights = {
              -- Highlight groups to override, adding new groups is also possible
              -- See `:h highlight-groups` for a list of highlight groups or run `:hi` to see all groups and their current values
              -- Example:
              Comment = { fg = "#696969", bg = "NONE", italic = true },
              -- Complete list can be found in `lua/cyberdream/theme.lua`
          },
          -- Override a highlight group entirely using the color palette
          overrides = function(colors) -- NOTE: This function nullifies the `highlights` option
              -- Example:
              return {
                  ["@property"] = { fg = colors.magenta, bold = true },
              }
          end,
          -- Override a color entirely
          colors = {
              -- For a list of colors see `lua/cyberdream/colours.lua`
              -- Example:
              bg = "#000000",
              green = "#00ff00",
              magenta = "#ff00ff",
          },
      },
      -- Disable or enable colorscheme extensions
      extensions = {
        alpha = true,
        cmp = true,
        dashboard = true,
        fzflua = true,
        gitpad = true,
        gitsigns = true,
        grapple = true,
        grugfar = true,
        heirline = true,
        helpview = true,
        hop = true,
        indentblankline = true,
        kubectl = true,
        lazy = true,
        leap = true,
        markdown = true,
        markview = true,
        mini = true,
        noice = true,
        neogit = true,
        notify = true,
        rainbow_delimiters = true,
        telescope = true,
        treesitter = true,
        treesittercontext = true,
        trouble = true,
        whichkey = true,
      },
    }
  },
  {
    "EdenEast/nightfox.nvim",
    opts = {
      options = {
        -- Compiled file's destination location
        compile_path = vim.fn.stdpath("cache") .. "/nightfox",
        compile_file_suffix = "_compiled", -- Compiled file suffix
        transparent = false,     -- Disable setting background
        terminal_colors = true,  -- Set terminal colors (vim.g.terminal_color_*) used in `:terminal`
        dim_inactive = false,    -- Non focused panes set to alternative background
        module_default = true,   -- Default enable value for modules
        colorblind = {
          enable = false,        -- Enable colorblind support
          simulate_only = false, -- Only show simulated colorblind colors and not diff shifted
          severity = {
            protan = 0,          -- Severity [0,1] for protan (red)
            deutan = 0,          -- Severity [0,1] for deutan (green)
            tritan = 0,          -- Severity [0,1] for tritan (blue)
          },
        },
        styles = {               -- Style to be applied to different syntax groups
          comments = "NONE",     -- Value is any valid attr-list value `:help attr-list`
          conditionals = "NONE",
          constants = "NONE",
          functions = "NONE",
          keywords = "NONE",
          numbers = "NONE",
          operators = "NONE",
          strings = "NONE",
          types = "NONE",
          variables = "NONE",
        },
        inverse = {             -- Inverse highlight for different types
          match_paren = false,
          visual = false,
          search = false,
        },
        modules = {             -- List of various plugins and additional options
          -- ...
        },
      },
      palettes = {},
      specs = {},
      groups = {},
    }
  },
  {
    "folke/tokyonight.nvim",
    lazy = false,
    priority = 1000,
    opts = {},
  }
}
