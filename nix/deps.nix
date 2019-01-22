{ stdenv, pkgs, buildErlangMk, buildRebar3, autoreconfHook, fetchHex, fetchFromGitHub, zlib, openssl_1_1, expat, libxml2, pc, ... }:

rec {
  cowlib = buildErlangMk {
      name = "cowlib";
      version = "2.7.0";
      src = fetchHex {
        pkg = "cowlib";
        version = "2.7.0";
        sha256 = "13nq8dnxrp5avqfwyz3w4yabc6v79a5xnvr2adpim4mr0kam5yar";
      };

      meta = {
        description = ''Support library for manipulating Web
                      protocols.'';
        license = stdenv.lib.licenses.isc;
        homepage = "https://github.com/ninenines/cowboy";
      };
    };
  ranch = buildErlangMk {
    name = "ranch";
    version = "1.7.1";
    src = fetchHex {
      pkg = "ranch";
      version = "1.7.1";
      sha256 = "1my7mz3x7a1fmjyin55nn1fr2d2rl3y64qf3kpcidxvxg0kqa7a5";
    };

    meta = {
      description = ''Socket acceptor pool for TCP protocols.'';
      license = stdenv.lib.licenses.isc;
      homepage = "https://github.com/ninenines/ranch";
    };
  };
  cowboy = buildErlangMk rec {
    name = "cowboy";
    version = "2.6.1";
    src = fetchFromGitHub {
      owner = "cowboy";
      repo = "cowboy";
      rev = "2.6.1";
      sha256 = "0q2383pdapakdzsmnq4pnj3qvlhsk3h6xchnfc4n60297z2zhz84";
    };
    beamDeps = [ cowlib ranch ];
  };
  lhttpc =
    buildRebar3 {
      name = "lhttpc";
      version = "1.5.5";
      src = fetchHex {
        pkg = "lhttpc";
        version = "1.5.5";
        sha256 =
          "0dda026c9c3860ef9015196add32c85c74fdfc8547df5e3dcdf827af555fcda6";
      };

      meta = {
        description = ''Lightweight HTTP Client'';
        license = stdenv.lib.licenses.bsd3;
        homepage = "https://github.com/erlcloud/lhttpc";
      };
    };
  epgsql =
    buildRebar3 {
      name = "epgsql";
      version = "4.1.0";
      src = fetchHex {
        pkg = "epgsql";
        version = "4.1.0";
        sha256 = "098c597ea0c25e995e6cad537e53b1742472ea61d68f98fe01cb1618fe5de595";
      };

      meta = {
        description = ''PostgreSQL Client'';
        license = stdenv.lib.licenses.bsd3;
        homepage = "https://github.com/epgsql/epgsql";
      };
    };
  poolboy =
    buildRebar3 {
      name = "poolboy";
      version = "1.5.1";
      src = fetchHex {
        pkg = "poolboy";
        version = "1.5.1";
        sha256 =
          "8f7168911120e13419e086e78d20e4d1a6776f1eee2411ac9f790af10813389f";
      };

      meta = {
        description = ''A hunky Erlang worker pool factory'';
        license = with stdenv.lib.licenses; [ unlicense asl20 ];
        homepage = "https://github.com/devinus/poolboy";
      };
    };
  pc = buildRebar3 {
    name = "pc";
    version = "1.10.0";
    src = fetchHex {
      pkg = "pc";
      version = "1.10.0";
      sha256 = "0kkbjjsvcrw2ql7dj28xgrsfrf6kbqzlfvjg3icj8wbasmvdbfrg";
    };

    meta = {
      description = ''a rebar3 port compiler for native code'';
      license = stdenv.lib.licenses.mit;
      homepage = "https://github.com/blt/port_compiler";
    };
  };
  exmpp =
    buildRebar3 {
      name = "exmpp";
      version = "unstable-0.9.4";
      src = fetchFromGitHub {
        owner = "Teknogrebo";
        repo = "exmpp";
        rev = "rebar";
        sha256 = "1clbqjsjk3jv6kdsby4njaipxl6g0pl3c85d6yb7gp6qkd30dfl8";
      };

      compilePorts = true;
      inherit pc;
      autoreconfFlags = "-vif";
      buildInputs = [ zlib openssl_1_1 expat libxml2 ];
      patches = [ ./exmpp.patch ];
    };
}
