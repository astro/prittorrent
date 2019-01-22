{ nixpkgs ? import ../../../programs/nixpkgs {} }:
nixpkgs.pkgs.callPackage ./nix/prittorrent.nix {}
