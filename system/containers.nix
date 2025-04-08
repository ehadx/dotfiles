{ config, pkgs, ... }:

{
  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
  };
  environment.systemPackages = with pkgs; [
    #    minikube
    #    kubectl
    #    kubernetes-helm
    distrobox
    podman-desktop
  ];
}
