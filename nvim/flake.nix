{
  description = "My nvim configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }:
    {
      system-conf = { config, pkgs, ...} : {
        environment.systemPackages = with pkgs; [
          neovim
        ];
      };

      home-conf = { config, pkgs, ... }:
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
              which-key-nvim
              nvim-treesitter
              nvim-treesitter-parsers.fsharp
              { plugin = nightfox-nvim;
                config = "colorscheme carbonfox";
              }
              { plugin = lualine-nvim;
                type = "lua";
                config = builtins.readFile ./lualine.lua;
              }
              { plugin = nvim-cmp;
                type = "lua";
                config = builtins.readFile ./cmp.lua;
              }
              { plugin = nvim-lspconfig;
                type = "lua";
                config = builtins.readFile ./lsp.lua;
              }
              { plugin = Ionide-vim;
                config = ''
                  let g:fsharp#fsautocomplete_command =
                    \ [ 'dotnet',
                    \   'fsautocomplete',
                    \   '--background-service-enabled'
                    \ ]
                '';
              }
            ];
          };
        };
    };
}
