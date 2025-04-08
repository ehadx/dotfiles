require("fzf-lua").setup {
  winopts = {
    preview = {
      hidden = true
    }
  }
}

local opts = { noremap = true, silent = true }
vim.keymap.set("n", "<C-x><C-f>", ":FzfLua files\n", opts)
vim.keymap.set("n", "<C-x><C-b>", ":FzfLua buffers\n", opts)
vim.keymap.set("n", "<C-x><C-g>", ":FzfLua grep\n", opts)
