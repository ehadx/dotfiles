{ config, pkgs, ... }:

{
  virtualisation = {
    docker = {
      enable = true;
      enableOnBoot = false;
    };
  };

  environment.systemPackages = with pkgs; [
    minikube
    kubectl
    kubernetes-helm
  ];
}
