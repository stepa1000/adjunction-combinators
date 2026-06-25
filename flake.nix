{
  description = "my project description";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs-old.url = "github:nixos/nixpkgs/nixos-25.11";

  outputs = { self, nixpkgs, flake-utils, nixpkgs-old}:
    flake-utils.lib.eachDefaultSystem (system:
      let
         
        #oldPkg = nixpkgs-old.legacyPackages.${system}; 

        pkgs = nixpkgs.legacyPackages.${system}; # import nixpkgs {

        hPkgs =
          pkgs.haskell.packages."ghc9124"; # "ghc9124"; # "ghc914"; # need to match Stackage LTS version
                                           # from stack.yaml snapshot
        # hPOld = pkgs.haskell.packages."ghc98";

        # hP10 = pkgs.haskell.packages."ghc910"; 

        myDevTools = [
          hPkgs.ghc # GHC compiler in the desired version (will be available on PATH)
	  # hPkgs.ghc-internal
          # hPkgs.ghcid # Continuous terminal Haskell compile checker
          # hPkgs.ormolu # Haskell formatter
          # hPkgs.hlint # Haskell codestyle checker
          hPkgs.hoogle # Lookup Haskell documentation
          # hPkgs.haskell-language-server # LSP server for editor
	  #oldPkg.haskellPackages.haskell-debugger
          # hPkgs.implicit-hie # auto generate LSP hie.yaml file from cabal
          # hPkgs.retrie # Haskell refactoring tool
          # hPkgs.cabal-install
          stack-wrapped
	  # pkgs.stack
	  hPkgs.cabal-install
          pkgs.zlib # External C library needed by some Haskell packages
	  hPkgs.zlib
          hPkgs.OpenGL
          pkgs.freeglut
          hPkgs.gl
	  hPkgs.GLUT
	  hPkgs.GLUtil
          pkgs.libGL
          pkgs.libGLU
	  pkgs.freeglut
          pkgs.blas
          pkgs.cairo.dev
          pkgs.file
          pkgs.gcc
          pkgs.glib.dev
          pkgs.gmp
          pkgs.gnumake
          pkgs.haskellPackages.stack
          pkgs.iana-etc
          pkgs.liblapack
          pkgs.pango.dev
          pkgs.perl
          # pkgs.pkgconfig
          pkgs.ncurses
          pkgs.zeromq
          pkgs.zlib.dev
	  pkgs.libgmpris
	  pkgs.libffi
	  pkgs.libelf
	  pkgs.libdwarf
	  pkgs.numactl
	  pkgs.elfutils
	  pkgs.alex
	  hPkgs.c2hs
	  hPkgs.cpphs
	  pkgs.doctest
	  #hPkgs.ghcjs
	  #pkgs.ghcjs-pkg
	  #hPkgs.greencard
	  pkgs.happy
	  #hPkgs.haskell-suite
	  #pkgs.haskell-suite-pkg
	  #hPkgs.hmake
	  #hPkgs.jhc
	  #hPkgs.uhc
	  #pkgs.pkg-config-unwrapped
          #hPkgs.pantry_0_11_2 
	  #hPkgs.text_2_1_4

	  ];

        # Wrap Stack to work with our Nix integration. We don't want to modify
        # stack.yaml so non-Nix users don't notice anything.
        # - no-nix: We don't want Stack's way of integrating Nix.
        # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
        # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
        
        
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack"; # will be available as the usual `stack` in terminal
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
	        --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };
      in {
        profile = ''
          export C_INCLUDE_PATH=/usr/include:$C_INCLUDE_PATH
  ''      ;
        devShells.default = pkgs.mkShell {
	  # packages = p: [ ];
          buildInputs = myDevTools;
          # Make external Nix c libraries like zlib known to GHC, like
          # pkgs.haskell.lib.buildStackProject does
          # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;
        };
      });
}

