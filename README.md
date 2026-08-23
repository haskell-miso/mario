# :ramen: 🍄 miso-mario

A side-scrolling World 1-1 written with [miso](https://github.com/dmjio/miso),
compiled to WebAssembly.

* Arrow keys / WASD to move, Up / W / Space to jump (hold for a higher jump)
* Bump `?` blocks from below, collect coins, mind the pits, reach the flag
* Frame-rate independent physics driven by `requestAnimationFrame` (`rAFSub`)
* Tracks miso `master`

## Build and run

Install [Nix Flakes](https://nixos.wiki/wiki/Flakes), then:

```
nix develop .#wasm
make
make serve
```

For a quick native type-check build (no WASM):

```
nix develop --command cabal build
```

On `x86_64-darwin`, `nixpkgs-unstable` no longer supports the platform, so
`nix develop` can't fetch its `bashInteractive` and falls back to macOS's
bash 3.2 (which fails with `syntax error near unexpected token ;&`). Work
around it with:

```
nix develop --override-flake nixpkgs github:NixOS/nixpkgs/nixos-25.11 --command cabal build
```
