{
  description = "majority-multisign";

  inputs = {
    haskell-nix.url = github:input-output-hk/haskell.nix;
    nixpkgs.follows = "haskell-nix/nixpkgs";
    haskell-nix-extra-hackage = {
      url = github:mlabs-haskell/haskell-nix-extra-hackage/separate-hackages;
      inputs = {
        haskell-nix.follows = "haskell-nix";
        nixpkgs.follows = "nixpkgs";
      };
    };

    cardano-addresses = {
      url = github:input-output-hk/cardano-addresses;
      flake = false;
    };
    cardano-base = {
      url = github:input-output-hk/cardano-base;
      flake = false;
    };
    cardano-crypto = {
      url = github:input-output-hk/cardano-crypto;
      flake = false;
    };
    cardano-ledger = {
      url = github:input-output-hk/cardano-ledger/node/1.33.0; #31276d4d47cf4e6a100e45958dbe93af5aed9208;
      flake = false;
    };
    cardano-node = {
      url = github:input-output-hk/cardano-node/1.33.0; #41afffd647fd92b731ebe96aaadf1dadfaaceba8;
      flake = false;
    };
    cardano-prelude = {
      url = github:input-output-hk/cardano-prelude;
      flake = false;
    };
    cardano-wallet = {
      url = github:input-output-hk/cardano-wallet;
      flake = false;
    };
    goblins = {
      url = github:input-output-hk/goblins;
      flake = false;
    };
    iohk-monitoring-framework = {
      url = github:input-output-hk/iohk-monitoring-framework;
      flake = false;
    };
    io-sim = {
      url = github:input-output-hk/io-sim;
      flake = false;
    };
    optparse-applicative-fork = {
      url = github:input-output-hk/optparse-applicative;
      flake = false;
    };
    ouroboros-network = {
      url = github:input-output-hk/ouroboros-network/236a0a289dd58ea510c77e9ba14b7465d117cb5e;
      flake = false;
    };
    plutus = {
      url = github:input-output-hk/plutus;
      flake = false;
    };
    plutus-apps = {
      url = github:input-output-hk/plutus-apps/27820e59a8239ea87b0b3c83c5bd37292de25667;
      flake = false;
    };
    quickcheck-dynamic = {
      url = github:input-output-hk/quickcheck-dynamic;
      flake = false;
    };
    typed-protocols = {
      url = github:input-output-hk/typed-protocols;
      flake = false;
    };
    Win32-network = {
      url = github:input-output-hk/Win32-network;
      flake = false;
    };
  };

  outputs = inputs@{ haskell-nix-extra-hackage, nixpkgs, haskell-nix, ... }:
    let
      supportedSystems =
        [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      pkgsFor = system: import nixpkgs {
        inherit system;
        inherit (haskell-nix) config;
        overlays = overlaysFor system;
      };

      compiler-nix-name = "ghc8107";
  
      overlayFor = system: final: prev:
        let myHackages = haskell-nix-extra-hackage.mkHackagesFor system compiler-nix-name
              (with inputs; [
                "${cardano-addresses}/core"

                "${cardano-base}/base-deriving-via"
                "${cardano-base}/binary"
                "${cardano-base}/binary/test"
                "${cardano-base}/cardano-crypto-class"
                "${cardano-base}/cardano-crypto-praos"
                "${cardano-base}/cardano-crypto-tests"
                "${cardano-base}/measures"
                "${cardano-base}/orphans-deriving-via"
                "${cardano-base}/slotting"
                "${cardano-base}/strict-containers"

                cardano-crypto

                "${cardano-ledger}/eras/alonzo/impl"
                "${cardano-ledger}/eras/alonzo/test-suite"
                # "${cardano-ledger}/eras/babbage/impl"
                # "${cardano-ledger}/eras/babbage/test-suite"
                "${cardano-ledger}/eras/byron/chain/executable-spec"
                "${cardano-ledger}/eras/byron/ledger/executable-spec"
                "${cardano-ledger}/eras/byron/ledger/impl"
                "${cardano-ledger}/eras/byron/ledger/impl/test"
                "${cardano-ledger}/eras/byron/crypto"
                "${cardano-ledger}/eras/byron/crypto/test"
                "${cardano-ledger}/eras/shelley/impl"
                "${cardano-ledger}/eras/shelley/test-suite"
                "${cardano-ledger}/eras/shelley-ma/impl"
                "${cardano-ledger}/eras/shelley-ma/test-suite"
                "${cardano-ledger}/libs/cardano-ledger-core"
                "${cardano-ledger}/libs/cardano-ledger-pretty"
                "${cardano-ledger}/libs/cardano-ledger-test"
                "${cardano-ledger}/libs/cardano-protocol-tpraos"
                "${cardano-ledger}/libs/plutus-preprocessor"
                "${cardano-ledger}/libs/ledger-state"
                "${cardano-ledger}/libs/non-integral"
                "${cardano-ledger}/libs/small-steps"
                "${cardano-ledger}/libs/small-steps-test"
                "${cardano-ledger}/libs/cardano-data"
                "${cardano-ledger}/libs/set-algebra"
                # "${cardano-ledger}/libs/vector-map"

                "${cardano-node}/cardano-api"

                "${cardano-prelude}/cardano-prelude"
                "${cardano-prelude}/cardano-prelude-test"

                "${cardano-wallet}/lib/cli"
                "${cardano-wallet}/lib/core"
                "${cardano-wallet}/lib/core-integration"
                "${cardano-wallet}/lib/dbvar"
                "${cardano-wallet}/lib/launcher"
                "${cardano-wallet}/lib/numeric"
                "${cardano-wallet}/lib/shelley"
                "${cardano-wallet}/lib/strict-non-empty-containers"
                "${cardano-wallet}/lib/test-utils"
                "${cardano-wallet}/lib/text-class"

                goblins

                "${iohk-monitoring-framework}/contra-tracer"
                "${iohk-monitoring-framework}/iohk-monitoring"
                "${iohk-monitoring-framework}/tracer-transformers"

                "${io-sim}/io-classes"
                "${io-sim}/io-sim"
                "${io-sim}/strict-stm"

                optparse-applicative-fork

                "${ouroboros-network}/ouroboros-network-testing"
                "${ouroboros-network}/monoidal-synchronisation"
                "${ouroboros-network}/network-mux"
                "${ouroboros-network}/ouroboros-network-framework"
                "${ouroboros-network}/ouroboros-network"
                "${ouroboros-network}/ouroboros-network-testing"
                "${ouroboros-network}/ouroboros-consensus"
                "${ouroboros-network}/ouroboros-consensus-byron"
                "${ouroboros-network}/ouroboros-consensus-byron-test"
                "${ouroboros-network}/ouroboros-consensus-byronspec"
                "${ouroboros-network}/ouroboros-consensus-cardano"
                "${ouroboros-network}/ouroboros-consensus-cardano-test"
                "${ouroboros-network}/ouroboros-consensus-mock"
                "${ouroboros-network}/ouroboros-consensus-mock-test"
                "${ouroboros-network}/ouroboros-consensus-protocol"
                "${ouroboros-network}/ouroboros-consensus-shelley"
                "${ouroboros-network}/ouroboros-consensus-shelley-test"
                "${ouroboros-network}/ouroboros-consensus-test"
                "${ouroboros-network}/ntp-client"
                "${ouroboros-network}/cardano-client"

                "${plutus}/plutus-benchmark"
                "${plutus}/plutus-conformance"
                "${plutus}/plutus-core"
                "${plutus}/plutus-errors"
                "${plutus}/plutus-ledger-api"
                "${plutus}/plutus-metatheory"
                "${plutus}/plutus-tx"
                "${plutus}/plutus-tx-plugin"
                "${plutus}/prettyprinter-configurable"
                "${plutus}/word-array"

                "${plutus-apps}/freer-extras"
                "${plutus-apps}/playground-common"
                "${plutus-apps}/plutus-chain-index"
                "${plutus-apps}/plutus-chain-index-core"
                "${plutus-apps}/plutus-contract"
                "${plutus-apps}/plutus-example"
                "${plutus-apps}/plutus-contract-certification"
                "${plutus-apps}/plutus-ledger"
                "${plutus-apps}/plutus-ledger-constraints"
                "${plutus-apps}/plutus-pab"
                "${plutus-apps}/plutus-pab-executables"
                "${plutus-apps}/plutus-playground-server"
                "${plutus-apps}/plutus-use-cases"

                quickcheck-dynamic

                "${typed-protocols}/typed-protocols"
                "${typed-protocols}/typed-protocols-cborg"
                "${typed-protocols}/typed-protocols-examples"

                Win32-network
              ]);
         in
        {
          majority-multisign = final.haskell-nix.cabalProject' {
            src = ./majority-multisign;
            inherit compiler-nix-name;
            index-state = "2022-07-13T00:00:00Z";

            configureArgs = ''
              --allow-newer=size-based:template-haskell
            '';

            inherit (myHackages) extra-hackages extra-hackage-tarballs modules;

            shell = {
              exactDeps = true;
              tools = { cabal-install = {}; };
            };
          };
        };

        overlaysFor = system: [ haskell-nix.overlay (overlayFor system) ];
    in {
      packages = perSystem (system: {
        default = ((pkgsFor system).majority-multisign.flake {}).packages."majority-multisign:lib:majority-multisign";
      });

      # checks = perSystem (system: self.flake.${system}.checks);
      # check = perSystem (system:
      #   (nixpkgsFor system).runCommand "combined-test" {
      #     nativeBuildInputs = builtins.attrValues self.checks.${system};
      #   } "touch $out");
      devShells = perSystem (system: {
        default = ((pkgsFor system).majority-multisign.flake {}).devShell;
      });

      overlays = perSystem (system: { default = overlayFor system; });
    };
}
