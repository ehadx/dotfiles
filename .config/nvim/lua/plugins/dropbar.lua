return {
  'Bekaboo/dropbar.nvim',
  -- optional, but required for fuzzy finder support
  dependencies = {
    'nvim-telescope/telescope-fzf-native.nvim'
  },
  config = function()
    local opts = { noremap = true, silent = true }
    local dropbar = require('dropbar.api')
    vim.keymap.set('n', '<leader>p', dropbar.pick, opts) 
  end
}
