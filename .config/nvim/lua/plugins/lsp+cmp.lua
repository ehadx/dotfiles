return {
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-buffer",
      'hrsh7th/cmp-cmdline',
    },
    config = function()
      local cmp = require("cmp")
      local compare = require('cmp.config.compare')

      cmp.setup({
        snippet = {
          -- REQUIRED - you must specify a snippet engine
          expand = function(args)
            vim.snippet.expand(args.body)
          end,
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

          require("config.keymaps").setup_lsp_keymaps(bufnr)
        end
        return on_attach
      end

      local servers = {
        bashls = true,
        html = {
          init_options = {
            configurationSection = { "html", "css", "javascript" },
            embeddedLanguages = {
              css = true,
              javascript = true
            },
            provideFormatter = true
          }
        },
        twiggy_language_server = true,
        jdtls = true,
        lua_ls = {
          on_init = function(client)
            local path = client.workspace_folders[1].name

            if vim.loop.fs_stat(path..'/.luarc.json') then
              return
            end

            if vim.loop.fs_stat(path..'/.luarc.jsonc') then
              return
            end

            client.config.settings.Lua = vim.tbl_deep_extend('force', client.config.settings.Lua, {
              runtime = {
                version = 'LuaJIT'
              },
              workspace = {
                checkThirdParty = false,
                library = {
                  vim.env.VIMRUNTIME
                },
                -- library = vim.api.nvim_get_runtime_file("", true)
              }
            })
          end,
          settings = {
            Lua = {}
          },
          server_capabilities = {
            semanticTokensProvider = vim.NIL,
          },
        },
        pyright = true,
        rust_analyzer = true,
        clangd = true,
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
