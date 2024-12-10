local lspconfig = require "lspconfig"

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
  end
  return on_attach
end

local servers = {
  bashls = true,
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


local on_attach = function(client, bufnr)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gD', "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gd', "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gm', "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gt', "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gr', "<cmd>lua vim.lsp.buf.references()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gh', "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gs', "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>lr', "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>la', "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>lf', "<cmd>lua vim.lsp.buf.format()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<space>ld", "<cmd>lua vim.diagnostic.open_float()<CR>", opts)
end

vim.cmd [[ let g:fsharp#lsp_auto_setup = 0 ]]

require('ionide').setup {
  autostart = true,
  on_attach = on_attach,
  flags = {
    debounce_text_changes = 150,
  },
  capabilities = capabilities,
}

