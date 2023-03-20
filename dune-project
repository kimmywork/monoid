(lang dune 3.0)

(name occam)

(package
 (name occam)
 (allow_empty)
 (depends ocaml dune))


(using menhir 2.0)
(using directory-targets 0.1)
; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
