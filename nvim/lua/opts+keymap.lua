vim.opt.backup = false
vim.opt.clipboard = "unnamedplus"
vim.opt.completeopt = { "menuone", "noselect", "popup" }
vim.opt.conceallevel = 0
vim.opt.fileencoding = "utf-8"
vim.opt.hlsearch = true
vim.opt.ignorecase = true
vim.opt.mouse = "a"
vim.opt.showtabline = 1
vim.opt.pumheight = 10
vim.opt.showmode = false
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.swapfile = true
vim.opt.termguicolors = true
vim.opt.timeoutlen = 300
vim.opt.undofile = true
vim.opt.updatetime = 300
vim.opt.writebackup = false
vim.opt.smartindent = true
vim.opt.smartcase = true
vim.opt.expandtab = true
vim.opt.shiftwidth = 4
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.numberwidth = 4
vim.opt.cursorline = false
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.signcolumn = "yes"
vim.opt.wrap = true
vim.opt.linebreak = true
vim.opt.scrolloff = 10
vim.opt.sidescrolloff = 8
vim.opt.foldlevel = 99
vim.opt.guifont = "monospace:h17"
vim.opt.whichwrap = "bs<>[]hl"
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
vim.wo.foldmethod = 'indent'
vim.wo.foldexpr = 'v:lua.vim.treesitter.foldexpr()'

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

local function end_of_line()
    local cursor = vim.api.nvim_win_get_cursor(0)
    vim.api.nvim_win_set_cursor(0, { cursor[1], vim.v.maxcol })
end

local function start_of_line()
    local cursor = vim.api.nvim_win_get_cursor(0)
    vim.api.nvim_win_set_cursor(0, { cursor[1], 0 })
end

local function start_of_indent()
    local var = vim.fn.getline '.':find '%S'
    local cursor = vim.api.nvim_win_get_cursor(0)
    vim.api.nvim_win_set_cursor(0, { cursor[1], var - 1 })
end

-- EMACS COMPAT
vim.keymap.set({ "n", "v" }, "<M-x>", ":", { noremap = true, silent = false })
vim.keymap.set({ "n", "v" }, "<C-e>", "$", opts)
vim.keymap.set("i", "<C-e>", end_of_line)
vim.keymap.set({ "n", "v" }, "<C-a>", "0", opts)
vim.keymap.set("i", "<C-a>", start_of_line)
vim.keymap.set({ "n", "v" }, "<M-m>", "^", opts)
vim.keymap.set("i", "<M-m>", start_of_indent)
vim.keymap.set("v", "<M-w>", "y", opts)
vim.keymap.set("i", "<C-d>", "<DEL>", opts)
vim.keymap.set("n", "<M-<>", 'gg', opts)
vim.keymap.set("n", "<M->>", 'G', opts)
vim.keymap.set({ "n", "v" }, "<C-f>", 'l', opts)
vim.keymap.set({ "n", "v" }, "<C-b>", 'h', opts)
vim.keymap.set("i", "<C-f>", "<Right>", opts)
vim.keymap.set("i", "<C-b>", "<Left>", opts)
vim.keymap.set("n", "<M-f>", 'w', opts)
vim.keymap.set("n", "<M-b>", 'b', opts)
vim.keymap.set({ 'n', 'v', 'i' }, "<C-g>", "<ESC>", opts)
vim.keymap.set({ 'n' }, "<C-x><C-s>", ":w\n", opts)
vim.keymap.set({ 'n' }, "<C-s>", "/", opts)
vim.keymap.set({ 'n' }, "<M-%>", ":%s/", { noremap = true, silent = false })
vim.keymap.set('n', "<C-x>o", "<C-w>w", opts)
vim.keymap.set('n', "<C-x>0", "<C-w>=", opts)
vim.keymap.set('n', "<C-x>1", "<C-w>_", opts)
vim.keymap.set('n', "<C-x>2", "<C-w>s", opts)
vim.keymap.set('n', "<C-x>3", "<C-w>v", opts)
vim.keymap.set('n', "<C-x>d", ":Explore\n", opts)

vim.g.mapleader = " "
vim.g.maplocalleader = " "
vim.g.netrw_banner = 0
vim.g.netrw_liststyle = 1

local term_opts = { silent = true }
vim.keymap.set('t', '<C-\\>', [[<C-\><C-n>]], term_opts)
vim.keymap.set('t', '<C-w>', [[<C-\><C-n><C-w>]], term_opts)

local function no_line_numbers()
    vim.opt.number = false
    vim.opt.relativenumber = false
end

vim.api.nvim_create_autocmd("TermOpen", {
  group = vim.api.nvim_create_augroup("custom-term-open", { clear = true }),
  callback = no_line_numbers,
})
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "c", "cpp" },
  group = vim.api.nvim_create_augroup("tab-opts", { clear = true }),
  callback = function (_)
    vim.opt_local.tabstop = 4
    vim.opt_local.shiftwidth = 4
    vim.opt_local.softtabstop = 4
  end
})

vim.api.nvim_create_autocmd("TermOpen", {
    group = vim.api.nvim_create_augroup("custom-term-open", { clear = true }),
    callback = no_line_numbers,
})

local st_job_id = 0
vim.keymap.set("n", "T", function()
    vim.cmd.vnew()
    vim.cmd.term()
    vim.cmd.wincmd("J")
    vim.api.nvim_win_set_height(0, 10)
    vim.api.nvim_feedkeys("A", "n", false)
    st_job_id = vim.bo.channel
    local is_windows = vim.fn.has("win64") == 1 or vim.fn.has("win32") == 1 or vim.fn.has("win16") == 1
    if is_windows then
        vim.fn.chansend(st_job_id, { "cls\r" })
    else
        vim.fn.chansend(st_job_id, { "clear\r\n" })
    end
end)
