return {
  'akinsho/toggleterm.nvim',
  version = "*",
  config = function()
    local opts = { noremap = true, silent = true }
    local Terminal = require('toggleterm.terminal').Terminal

    local lazygit = Terminal:new({
      cmd = 'lazygit',
      dir = 'git_dir',
      hidden = true,
      direction = 'float',
    })

    local lazydocker = Terminal:new({
      cmd = 'lazydocker',
      dir = 'git_dir',
      hidden = true,
      direction = 'float',
    })

    local btop = Terminal:new({
      cmd = 'btop',
      hidden = true,
      direction = 'float',
      highlights = {
        FloatBorder = { guibg = 'Black', guifg = 'DarkGray' },
        NormalFloat = { guibg = 'Black' },
      },
    })

    vim.keymap.set('n', '<leader>tb', function() btop:toggle() end, {
      desc = 'toggleterm: toggle btop',
    })
    vim.keymap.set('n', '<leader>tg', function() lazygit:toggle() end, {
      desc = 'toggleterm: toggle lazygit',
    })
    vim.keymap.set('n', '<leader>td', function() lazydocker:toggle() end, {
      desc = 'toggleterm: toggle lazydocker',
    })

    require("toggleterm").setup {
      open_mapping = [['<leader>t']],
    }

    vim.keymap.set('n', '<leader>tf', ':ToggleTerm direction=float<CR>', opts)
    vim.keymap.set('n', '<leader>th', ':ToggleTerm direction=horizontal<CR>', opts)
    vim.keymap.set('n', '<leader>tv', ':ToggleTerm size=90 direction=vertical<CR>', opts)
    vim.keymap.set('n', '<leader>tt', ':ToggleTerm direction=tab<CR>', opts)
    vim.keymap.set('n', '<leader>to', ':ToggleTermToggleAll<CR>', opts)
  end
}
