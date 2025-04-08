vim.lsp.config["*"] = {
    root_markers = { ".git" },
}

vim.api.nvim_create_autocmd("LspAttach", {
    group = vim.api.nvim_create_augroup("my.lsp", {}),
    callback = function(args)
        local client = assert(vim.lsp.get_client_by_id(args.data.client_id))
        local opts = { noremap = true, silent = true }
        vim.api.nvim_buf_set_keymap(args.buf, "n", "gD", ":lua vim.lsp.buf.declaration()<CR>", opts)
        vim.api.nvim_buf_set_keymap(args.buf, "n", "gd", ":lua vim.lsp.buf.definition()<CR>", opts)
        vim.api.nvim_buf_set_keymap(args.buf, "n", "gt", ":lua vim.lsp.buf.type_definition()<CR>", opts)
        vim.api.nvim_buf_set_keymap(args.buf, "n", "gr", ":lua vim.lsp.buf.references()<CR>", opts)
        vim.api.nvim_buf_set_keymap(args.buf, "n", "gh", ":lua vim.lsp.buf.hover()<CR>", opts)
        vim.api.nvim_buf_set_keymap(args.buf, "n", "gs", ":lua vim.lsp.buf.signature_help()<CR>", opts)
        vim.api.nvim_buf_set_keymap(args.buf, "n", "<leader>lr", ":lua vim.lsp.buf.rename()<CR>", opts)
        vim.api.nvim_buf_set_keymap(args.buf, "n", "<leader>la", ":lua vim.lsp.buf.code_action()<CR>", opts)
        vim.api.nvim_buf_set_keymap(args.buf, "n", "<leader>lf", ":lua vim.lsp.buf.format()<CR>", opts)
        vim.api.nvim_buf_set_keymap(args.buf, "n", "<leader>ld", ":lua vim.diagnostic.open_float()<CR>", opts)

        vim.lsp.inlay_hint.enable(true)

        if client:supports_method("textDocument/implementation") then
            vim.api.nvim_buf_set_keymap(args.buf, "n", "gm", ":lua vim.lsp.buf.implementation()<CR>", opts)
        end

        -- Enable auto-completion. Note: Use CTRL-Y to select an item. |complete_CTRL-Y|
        if client:supports_method("textDocument/completion") then
            -- false because we are using blink
            vim.lsp.completion.enable(false, client.id, args.buf, {
                autotrigger = true,
                -- Some convert method to control how the completions appears in the menu
                convert = function(item)
                    return { abbr = item.label:gsub("%b()", "") }
                end
            })
        end
        -- Auto-format ("lint") on save.
        -- Usually not needed if server supports "textDocument/willSaveWaitUntil".
        if not client:supports_method("textDocument/willSaveWaitUntil")
            and client:supports_method("textDocument/formatting") then
            -- Auto-format
            vim.api.nvim_create_autocmd("BufWritePre", {
                group = vim.api.nvim_create_augroup("my.lsp", { clear = false }),
                buffer = args.buf,
                callback = function()
                    if not client.name == "clangd" then
                        vim.lsp.buf.format({ bufnr = args.buf, id = client.id, timeout_ms = 1000 })
                    end
                end,
            })

            vim.api.nvim_buf_set_keymap(
                args.buf,
                "n",
                "<leader>lf",
                ":lua vim.lsp.buf.format({ bufnr = " ..
                args.buf .. ", id = " .. client.id .. ", timeout_ms = 1000 })<CR>",
                { noremap = true, silent = true }
            )
        end
    end,
})

vim.lsp.config["luals"] = {
    cmd = { "lua-language-server" },
    filetypes = { "lua" },
    root_markers = { ".luarc.json", ".luarc.jsonc", ".git" },
    -- Specific settings to send to the server. The schema for this is
    -- defined by the server. For example the schema for lua-language-server
    -- can be found here https://raw.githubusercontent.com/LuaLS/vscode-lua/master/setting/schema.json
    settings = {
        Lua = {
            runtime = {
                version = "LuaJIT",
            },
            workspace = {
                checkThirdParty = false,
                library = {
                    vim.env.VIMRUNTIME
                },
            }
        },
    }
}

vim.lsp.config["rust_analyzer"] = {
    cmd = { "rust-analyzer" },
    filetypes = { "rust" },
    settings = {
        ["rust-analyzer"] = {},
    }
}

vim.lsp.config["clangd"] = {
    cmd = {
        "clangd",
        "--background-index",
        "--clang-tidy",
        "--header-insertion=iwyu",
        "--completion-style=detailed",
        "--function-arg-placeholders",
        "--fallback-style=llvm",
    },
    single_file_support = true,
    init_options = {
        usePlaceholders = true,
        completeUnimported = true,
        clangdFileStatus = true,

    },
    keys = {
        { "<leader>ch", "<cmd>ClangdSwitchSourceHeader<cr>", desc = "Switch Source/Header (C/C++)" },
        { "<leader>ci", "<cmd>ClangdShowSymbolInfo<cr>",     desc = "Show Symbol Info" },
    },
    filetypes = { "c", "cpp", "objc", "objcpp", "cuda", "proto" },
    capabilities = {
        offsetEncoding = { "utf-8", "utf-16" },
        textDocument = {
            completion = {
                editsNearCursor = true
            },
        },
    },
    root_markers = {
        "Makefile",
        "configure.ac",
        "configure.in",
        "config.h.in",
        "meson.build",
        "meson_options.txt",
        "build.ninja",
        ".git",
    },
}

vim.lsp.enable "luals"
vim.lsp.enable "rust_analyzer"
vim.lsp.enable "clangd"
