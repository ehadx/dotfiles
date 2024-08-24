return {
  {
    "lewis6991/gitsigns.nvim",
    config = function()
      require('gitsigns').setup {
         numhl = true,
      }
    end
  },
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",         -- required
      "sindrets/diffview.nvim",        -- optional - Diff integration

      -- Only one of these is needed, not both.
      "nvim-telescope/telescope.nvim", -- optional
      -- "ibhagwan/fzf-lua",           -- optional
    },
    config = true
  }
}
