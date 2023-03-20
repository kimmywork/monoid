
```
opam switch create 5.0.0+options
opam repository add dune-universe git+https://github.com/dune-universe/opam-overlays.git

opam monorepo lock
opam monorepo pull

dune build
```
