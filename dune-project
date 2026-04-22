(lang dune 3.22)

(name simple-symb)

(generate_opam_files true)

(source
 (github francescoborri/simple-symb))

(authors "Francesco Borri")

(maintainers "Francesco Borri")

(license GPL-3.0)

(using menhir 3.0)

(package
 (name simple-symb)
 (synopsis "Symbolic interpreter for a simple imperative language")
 (description
  "Symbolic interpreter for a simple imperative language written using Soteria.")
 (depends
  ocaml
  dune
  menhir
  ppx_blob
  ppx_subliner
  soteria
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.29.0)))
  (ocaml-lsp-server :with-dev-setup)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
