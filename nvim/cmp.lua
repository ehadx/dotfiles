local cmp = require("cmp")
local compare = require("cmp.config.compare")

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
