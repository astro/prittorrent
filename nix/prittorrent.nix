{ stdenv, pkgs, autoreconfHook, makeWrapper, beam, fetchFromGitHub, zlib, openssl_1_1, expat, libxml2, which, ... }:

let
  erlangPackages = beam.packages.erlangR21;
  deps = import ./deps.nix (pkgs // erlangPackages);

  mkApp = app:
    with erlangPackages;
    buildRebar3 rec {
      name = "prittorrent-${app}";
      version = "1.0.0";
      src = ../.;
      buildInputs = [ makeWrapper ];
      beamDeps = with deps; [
        lhttpc
        epgsql
        exmpp
        poolboy cowboy
      ];
      installPhase = ''
          mkdir -p $out/opt
          HOME=. rebar3 release -n ${app} -o $out/opt

          mkdir -p $out/bin
          cat > $out/bin/${app} <<__EOF__
          #!/usr/bin/env bash
          set -e

          cd $out/opt/${app}
          exec ./bin/${app} $@
          __EOF__
          chmod a+x $out/bin/${app}

          wrapProgram $out/bin/${app} \
              --prefix PATH ":" "${which}/bin" \
              --prefix PATH ":" "${erlang}/bin"
      '';
    };
  prittorrent-feeds = mkApp "feeds";
  prittorrent-hasher = mkApp "hasher";
  prittorrent-seeder = mkApp "seeder";
  prittorrent = pkgs.buildEnv {
    name = "prittorrent";
    paths = [
      prittorrent-feeds
      prittorrent-hasher
      prittorrent-seeder
    ];
    pathsToLink = [ "/bin" ];
  };
in prittorrent
