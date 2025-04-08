local neogit = require('neogit')
neogit.setup {}

vim.keymap.set({ 'n' }, "<C-x>g", ":Neogit\n", { noremap = true, silent = true })
