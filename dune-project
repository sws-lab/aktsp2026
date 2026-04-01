(lang dune 3.0)
(name aktsp2026)

(source (github sws-lab/aktsp2026))
(authors "Simmo Saan")
(maintainers "Simmo Saan")

(generate_opam_files true)

(package
 (name aktsp2026)
 (synopsis "AKT süvenduspraktikumi 2026 materjalid")
 (depends
  ocamlgraph
  ppx_deriving
  ppx_deriving_hash
  z3
  ounit2
  (qcheck-ounit (>= 0.91))
  ocaml-lsp-server
  utop)
 (allow_empty))
