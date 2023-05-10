{
  inputs = {
    common.url = "github:nammayatri/common";
    nixpkgs.follows = "common/nixpkgs";
    flake-parts.follows = "common/flake-parts";
    systems.url = "github:nix-systems/default";

    passetto-hs.url = "github:juspay/passetto/bb92cf1dd9699662d2a7bb96cd6a6aed6f20e8ff";
    passetto-hs.flake = false;
    clickhouse-haskell.url = "github:piyushKumar-1/clickhouse-haskell";
    clickhouse-haskell.flake = false;
    wai-middleware-prometheus.url = "/home/roman/Projects/beckn/nammayatri/prometheus-juspay/prometheus-haskell";
    # wai-middleware-prometheus.url = "github:juspay/prometheus-haskell/more-proc-metrics";
    wai-middleware-prometheus.flake = false;

    euler-hs.url = "github:juspay/euler-hs";
  };
  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.common.flakeModules.default
      ];

      perSystem = { self', pkgs, lib, config, ... }: {
        haskellProjects.default = {
          imports = [
            inputs.euler-hs.haskellFlakeProjectModules.output
          ];
          source-overrides = {
            passetto-client = inputs.passetto-hs + /client;
            passetto-core = inputs.passetto-hs + /core;
            wai-middleware-prometheus = inputs.wai-middleware-prometheus + /wai-middleware-prometheus;
            prometheus-client = inputs.wai-middleware-prometheus + /prometheus-client;
            inherit (inputs) clickhouse-haskell;
          };
          overrides = self: super:
            with pkgs.haskell.lib.compose;
            lib.mapAttrs (k: lib.pipe super.${k}) {
              # Tests and documentation generation fail for some reason.
              euler-hs = [ dontCheck dontHaddock ];
              clickhouse-haskell = [ doJailbreak ];
            };
          autoWire = [ "packages" "checks" ];
        };
        packages.default = self'.packages.mobility-core;
        devShells.default = pkgs.mkShell {
          # cf. https://haskell.flake.page/devshell#composing-devshells
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.pre-commit.devShell
          ];
        };
      };
    };
}
