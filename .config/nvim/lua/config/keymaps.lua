local opts = { noremap = true, silent = true }
vim.keymap.set("", "<Space>", "<Nop>", opts)
vim.keymap.set("", "<C-c>", "<Nop>", opts)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Resize with arrows
vim.keymap.set("n", "<C-Up>", ":resize +2<CR>", opts)
vim.keymap.set("n", "<C-Down>", ":resize -2<CR>", opts)
vim.keymap.set("n", "<C-Left>", ":vertical resize +2<CR>", opts)
vim.keymap.set("n", "<C-Right>", ":vertical resize -2<CR>", opts)

-- Navigate buffers
vim.keymap.set("n", "]b", ":bnext<CR>", opts)
vim.keymap.set("n", "[b", ":bprevious<CR>", opts)
vim.keymap.set("n", "]c", ":cnext<CR>", opts)
vim.keymap.set("n", "[c", ":cprev<CR>", opts)
vim.keymap.set("n", "]l", ":lnext<CR>", opts)
vim.keymap.set("n", "[l", ":lprev<CR>", opts)
vim.keymap.set("n", "]t", ":tabnext<CR>", opts)
vim.keymap.set("n", "[t", ":tabprev<CR>", opts)
vim.keymap.set("n", "<C-c>b", ":bdelete<CR>", opts)
vim.keymap.set("n", "<C-c>t", ":tabclose<CR>", opts)
vim.keymap.set("n", "<C-c>c", ":cclose<CR>", opts)
vim.keymap.set("v", "p", '"_dP', opts)


vim.keymap.set({ 'i', 's' }, '<C-j>', function()
  return vim.snippet.active { direction = 1 } and vim.snippet.jump(1)
end, { expr = true, silent = true })
vim.keymap.set({ 'i', 's' }, '<C-k>', function()
  return vim.snippet.active { direction = -1 } and vim.snippet.jump(-1)
end, { expr = true, silent = true })


local term_opts = { silent = true }
vim.keymap.set('t', '<C-\\>', [[<C-\><C-n>]], term_opts)
vim.keymap.set('t', '<C-w>', [[<C-\><C-n><C-w>]], term_opts)


M = {}

M.setup_lsp_keymaps = function(bufnr)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gD', ":lua vim.lsp.buf.declaration()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gd', ":lua vim.lsp.buf.definition()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gm', ":lua vim.lsp.buf.implementation()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gt', ":lua vim.lsp.buf.type_definition()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gr', ":lua vim.lsp.buf.references()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gh', ":lua vim.lsp.buf.hover()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gs', ":lua vim.lsp.buf.signature_help()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>lr', ":lua vim.lsp.buf.rename()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>la', ":lua vim.lsp.buf.code_action()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>lf', ":lua vim.lsp.buf.format()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>ld", ":lua vim.diagnostic.open_float()<CR>", opts)

  local opts_copy = vim.tbl_deep_extend("force", opts, { buffer = 0 })
  local tbuiltins = require "telescope.builtin"

  vim.keymap.set('n', '<leader>fld', tbuiltins.lsp_definitions, opts_copy)
  vim.keymap.set('n', '<leader>flr', tbuiltins.lsp_references, opts_copy)
end

M.setup_telescope_keymaps = function()
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

return M
