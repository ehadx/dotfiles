return {
  'nvim-telescope/telescope.nvim',
  tag = '0.1.8',
  dependencies = { 'nvim-lua/plenary.nvim' },
  config = function()
    local opts = { noremap = true, silent = true }

    local builtin = require('telescope.builtin')
    vim.keymap.set('n', '<leader>ff', builtin.find_files, opts)
    vim.keymap.set('n', '<leader>fo', builtin.oldfiles, opts)
    vim.keymap.set('n', '<leader>fw', builtin.live_grep, opts)
    vim.keymap.set('n', '<leader>fv', builtin.vim_options, opts)
    vim.keymap.set('n', '<leader>fr', builtin.registers, opts)
    vim.keymap.set('n', '<leader>fb', builtin.buffers, opts)
    vim.keymap.set('n', '<leader>fc', builtin.commands, opts)
    vim.keymap.set('n', '<leader>fhh', builtin.help_tags, opts)
    vim.keymap.set('n', '<leader>fhl', builtin.highlights, opts)
    vim.keymap.set('n', '<leader>fhs', builtin.search_history, opts)
    vim.keymap.set('n', '<leader>fhc', builtin.command_history, opts)
    vim.keymap.set('n', '<leader>fmp', builtin.man_pages, opts)
    vim.keymap.set('n', '<leader>fmk', builtin.marks, opts)
  end
}
