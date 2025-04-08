require("blink.cmp").setup {
    keymap = {
        preset = 'default',
        ['C-space'] = {},

        -- not working, fixed manually below
        ['C-M-i'] = { "show", "show_documentation", "hide_documentation" },
    },
    signature = { enabled = true },

    appearance = {
        -- 'mono' (default) for 'Nerd Font Mono' or 'normal' for 'Nerd Font'
        -- Adjusts spacing to ensure icons are aligned
        nerd_font_variant = 'mono'
    },

    completion = {
        menu = {
            auto_show = false,
        },
        ghost_text = {
            enabled = true,
            show_with_menu = false,
        },
        -- (Default) Only show the documentation popup when manually triggered
        documentation = {
            auto_show = true
        }
    },

    -- Default list of enabled providers defined so that you can extend it
    -- elsewhere in your config, without redefining it, due to `opts_extend`
    sources = {
        default = { 'lsp', 'path', 'snippets', 'buffer' },
    },

    -- (Default) Rust fuzzy matcher for typo resistance and significantly better performance
    -- You may use a lua implementation instead by using `implementation = "lua"` or fallback to the lua implementation,
    -- when the Rust fuzzy matcher is not available, by using `implementation = "prefer_rust"`
    --
    -- See the fuzzy documentation for more information
    fuzzy = { implementation = "prefer_rust_with_warning" }
}

local function show()
    local blink = require("blink.cmp")

    if not blink.is_menu_visible() then
        blink.show()
    else
        blink.hide()
    end
end

vim.keymap.set("i", "<C-M-i>", show, { remap = true, silent = true })
