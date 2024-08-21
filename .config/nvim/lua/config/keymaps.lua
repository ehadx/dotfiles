local opts = { noremap = true, silent = true }

local term_opts = { silent = true }

local keymap = vim.keymap.set

keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Better window navigation
keymap("n", "<C-h>", "<C-w>h", opts)
keymap("n", "<C-j>", "<C-w>j", opts)
keymap("n", "<C-k>", "<C-w>k", opts)
keymap("n", "<C-l>", "<C-w>l", opts)

-- Resize with arrows
keymap("n", "<C-Up>", ":resize +2<CR>", opts)
keymap("n", "<C-Down>", ":resize -2<CR>", opts)
keymap("n", "<C-Left>", ":vertical resize +2<CR>", opts)
keymap("n", "<C-Right>", ":vertical resize -2<CR>", opts)

keymap('n', '|', ':vsplit<CR>', opts)
keymap('n', '\\', ':split<CR>', opts)

keymap('n', '<leader>=', '<C-w>=', opts)
keymap('n', '<leader>_', '<C-w>_', opts)
keymap('n', '<leader>|', '<C-w>|', opts)

-- Navigate buffers
keymap("n", "<leader>btn", ":tabnext<CR>", opts)
keymap("n", "<leader>btp", ":tabprevious<CR>", opts)
keymap("n", "<leader>btc", ":tabclose<CR>", opts)


keymap("n", "<leader>bn", ":bnext<CR>", opts)
keymap("n", "<leader>bp", ":bprevious<CR>", opts)
keymap("n", "<leader>bc", ":bdelete<CR>", opts)

keymap("v", "p", '"_dP', opts)

vim.keymap.set('t', '<C-\\>', [[<C-\><C-n>]], term_opts)
vim.keymap.set('t', '<C-h>', [[<Cmd>wincmd h<CR>]], term_opts)
vim.keymap.set('t', '<C-j>', [[<Cmd>wincmd j<CR>]], term_opts)
vim.keymap.set('t', '<C-k>', [[<Cmd>wincmd k<CR>]], term_opts)
vim.keymap.set('t', '<C-l>', [[<Cmd>wincmd l<CR>]], term_opts)
vim.keymap.set('t', '<C-w>', [[<C-\><C-n><C-w>]], term_opts)
