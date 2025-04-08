require "opts+keymap"
--require "lsp"

require "config.lazy"
require "config.mason"
require "config.blink_cmp"
require "config.treesitter"
require "config.lualine"
require "config.neogit"
require "config.fzf"

require "gruvbox".setup {
    contrast = "hard"
}

vim.cmd "colorscheme gruvbox"
