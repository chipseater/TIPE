(lang dune 3.10)

(name TIPE)

(generate_opam_files true)

(source
 (github chipseater/TIPE))

(authors "Eric de Malet (@chipseater)" "Sylvain Drault (@Astatre)")

(license Fair License)

(package
 (name TIPE)
 (synopsis "TIPE project - describing urban evolution using natural selection")
 (description "This personnal initiative project is meant to play around with a natural selection algotithm.")
 (depends ocaml dune tsdl)
 (tags
  ("TIPE" "natural evolution" "modelisation" "school project"))
)

