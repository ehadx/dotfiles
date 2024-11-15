{ config, lib, pkgs, ... }:

{
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    extraConfig = builtins.readFile ./tabline.vim;
    extraLuaConfig = builtins.readFile ./opts+keymap.lua;
    plugins = with pkgs.vimPlugins; [
      neogit
      vim-nix
      gitsigns-nvim
      cmp-nvim-lsp
      cmp-path
      cmp-buffer
      cmp-cmdline
      vim-dadbod
      vim-dadbod-ui
      vim-dadbod-completion
      { plugin = lualine-nvim;
        type = "lua";
        config = builtins.readFile ./lualine.lua;
      }
      { plugin = cyberdream-nvim;
        config = "colorscheme cyberdream";
      }
      { plugin = nvim-cmp;
        type = "lua";
        config = builtins.readFile ./cmp.lua;
      }
      { plugin = nvim-lspconfig;
        type = "lua";
        config = builtins.readFile ./lsp.lua;
      }
    ];
  };
}
