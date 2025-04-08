require "nvim-treesitter.configs".setup {
    ensure_installed = {
        "c",
        "lua",
        "zig",
        "rust",
        "vim",
        "vimdoc",
        "markdown",
        "markdown_inline",
    },
    highlight = {
        enable = true
    },
    incremental_selection = {
        enable = true,
        keymaps = {
            init_selection = "tnn",
            node_incremental = "trn",
            scope_incremental = "trc",
            node_decremental = "trm",
        },
    },
    indent = {
        enable = true
    },
}
