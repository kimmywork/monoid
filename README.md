
```
opam switch create 5.0.0+options
eval $(opam env --switch=5.0.0+options)
opam install . --deps-only --with-test
eval $(opam config env)

dune build
dune test
```
