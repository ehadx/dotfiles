return {
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "onsails/lspkind.nvim",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-buffer",
      'hrsh7th/cmp-cmdline',
      { "L3MON4D3/LuaSnip", build = "make install_jsregexp" },
      "saadparwaiz1/cmp_luasnip",
    },
    config = function()
      local cmp = require("cmp")
      local compare = require('cmp.config.compare')
      local lspkind = require "lspkind"

      cmp.setup({
        snippet = {
          -- REQUIRED - you must specify a snippet engine
          expand = function(args)
            -- vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
            require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
            -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
            -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
          end,
        },
        formatting = {
          format = lspkind.cmp_format({
            mode = 'symbol_text',
            maxwidth = 50,
            ellipsis_char = '...',
            show_labelDetails = true,
          }),
        },
        sorting = {
          priority_weight = 2,
          comparators = {
            compare.offset,
            compare.exact,
            compare.score,
            compare.recently_used,
            compare.kind,
            compare.sort_text,
            compare.length,
            compare.order,
          },
        },
        window = {
          completion = cmp.config.window.bordered(),
          documentation = cmp.config.window.bordered(),
        },
        mapping = cmp.mapping.preset.insert({
          ["<C-b>"] = cmp.mapping.scroll_docs(-4),
          ["<C-f>"] = cmp.mapping.scroll_docs(4),
          ['<C-n>'] = cmp.mapping(cmp.mapping.select_next_item(), { 'i', 'c' }),
          ['<C-p>'] = cmp.mapping(cmp.mapping.select_prev_item(), { 'i', 'c' }),
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<C-e>"] = cmp.mapping.abort(),
          ["<CR>"] = cmp.mapping.confirm({ select = true }),
        }),
        sources = cmp.config.sources({
          { name = "nvim_lsp" },
          { name = "luasnip" },
        }, {
          { name = "buffer" },
          { name = "path" },
        }),
      })

      cmp.setup.cmdline('/', {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({
          { name = "buffer" },
        }),
      })

      cmp.setup.cmdline(':', {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({
          { name = "path" },
        }, {
          {
            name = 'cmdline',
            option = {
              ignore_cmds = { 'Man', '!' }
            }
          }
        }),
      })
    end
  },
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
    },
    config = function()
      local lspconfig = require "lspconfig"
      require "mason".setup()

      local capabilities = nil
      if pcall(require, "cmp_nvim_lsp") then
        local client_capabilities = vim.lsp.protocol.make_client_capabilities()
        capabilities = require("cmp_nvim_lsp").default_capabilities(client_capabilities)
      end

      local opts = { noremap = true, silent = true }
      vim.keymap.set("n", "<leader>lD", vim.diagnostic.open_float, opts)
      vim.keymap.set("n", "<leader>ldn", vim.diagnostic.goto_next, opts)
      vim.keymap.set("n", "<leader>ldp", vim.diagnostic.goto_prev, opts)
      -- setloclist

      local on_attach_gen = function(name, server_capabilities)
        local on_attach = function(client, bufnr)
          if client.server_capabilities.inlayHintProvider then
            vim.g.inlay_hints_visible = true
          else
            print("no inlay hints available")
          end

          if client.name == name then
            if not server_capabilities == nil then
              for k, v in pairs(server_capabilities) do
                if v == vim.NIL then
                  v = nil
                end
                client.server_capabilities[k] = v
              end
            end
          end

          opts['buffer'] = 0
          vim.keymap.set('n', '<leader>lgD', vim.lsp.buf.declaration, opts)
          vim.keymap.set('n', '<leader>lgd', vim.lsp.buf.definition, opts)
          vim.keymap.set('n', '<leader>lgi', vim.lsp.buf.implementation, opts)
          vim.keymap.set('n', '<leader>lgt', vim.lsp.buf.type_definition, opts)
          vim.keymap.set('n', '<leader>lgr', vim.lsp.buf.references, opts)
          vim.keymap.set('n', '<leader>lh', vim.lsp.buf.hover, opts)
          vim.keymap.set('n', '<leader>ls', vim.lsp.buf.signature_help, opts)
          vim.keymap.set('n', '<leader>lr', vim.lsp.buf.rename, opts)
          vim.keymap.set('n', '<leader>la', vim.lsp.buf.code_action, opts)
          vim.keymap.set('n', '<leader>lf', vim.lsp.buf.format, opts)

          local tbuiltins = require "telescope.builtin"
          vim.opt_local.omnifunc = "v:lua.vim.lsp.omnifunc"
          vim.keymap.set('n', '<leader>fld', tbuiltins.lsp_definitions, opts)
          vim.keymap.set('n', '<leader>flr', tbuiltins.lsp_references, opts)
        end
        return on_attach
      end

      local servers = {
        bashls = true,
        lua_ls = {
          server_capabilities = {
            semanticTokensProvider = vim.NIL,
          },
        },
        ruff_lsp = true,
        rust_analyzer = true,
        clangd = {
          init_options = { clangdFileStatus = true },
          filetypes = { "c" }
        }
      }

      for name, config in pairs(servers) do
        if config == true then
          config = {}
        end

        config = vim.tbl_deep_extend("force", {}, {
          capabilities = capabilities,
          on_attach = on_attach_gen(name, config.server_capabilities)
        }, config)

        lspconfig[name].setup(config)
      end
    end,
  },
}
