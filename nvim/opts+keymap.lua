local options = {
  backup = false,
  clipboard = "unnamedplus",
  completeopt = { "menuone", "noselect", "popup" },
  conceallevel = 0,
  fileencoding = "utf-8",
  hlsearch = true,
  ignorecase = true,
  mouse = "a",
  pumheight = 10,
  showmode = false,
  showtabline = 2,
  splitbelow = true,
  splitright = true,
  swapfile = true,
  termguicolors = true,
  timeoutlen = 300,
  undofile = true,
  updatetime = 300,
  writebackup = false,
  smartindent = true,
  smartcase = true,
  expandtab = true,
  shiftwidth = 2,
  tabstop = 2,
  softtabstop = 2,
  cursorline = false,
  number = true,
  relativenumber = true,
  numberwidth = 4,
  signcolumn = "yes",
  wrap = true,
  linebreak = true,
  scrolloff = 10,
  sidescrolloff = 8,
  guifont = "monospace:h17",
  whichwrap = "bs<>[]hl",
}

for k, v in pairs(options) do
  vim.opt[k] = v
end

vim.opt.shortmess = "ilmnrx"                    -- flags to shorten vim messages, see :help 'shortmess'
vim.opt.shortmess:append "c"                    -- don't give |ins-completion-menu| messages
vim.opt.iskeyword:append "-"                    -- hyphenated words recognized by searches
vim.opt.formatoptions:remove({ "c", "r", "o" }) -- don't insert the current comment leader automatically
                                                -- for auto-wrapping comments using 'textwidth', hitting
                                                --  <Enter> in insert mode, or hitting 'o' or 'O' in
                                                --  normal mode.

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

vim.keymap.set("n", "<M-x>", ":", { noremap = true, silent = false })

local term_opts = { silent = true }
vim.keymap.set('t', '<C-\\>', [[<C-\><C-n>]], term_opts)
vim.keymap.set('t', '<C-w>', [[<C-\><C-n><C-w>]], term_opts)

function no_line_numbers()
  vim.opt.number = false
  vim.opt.relativenumber = false
end

vim.api.nvim_create_autocmd("TermOpen", {
  group = vim.api.nvim_create_augroup("custom-term-open", { clear = true }),
  callback = no_line_numbers,
})

local st_job_id = 0
vim.keymap.set("n", "<M-t>", function()
  vim.cmd.vnew()
  vim.cmd.term()
  vim.cmd.wincmd("J")
  vim.api.nvim_win_set_height(0, 10)
  vim.api.nvim_feedkeys("A", "n", false)
  st_job_id = vim.bo.channel
  vim.fn.chansend(st_job_id, { "clear\r\n" })
end)
