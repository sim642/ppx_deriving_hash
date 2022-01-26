(lang dune 2.8)
(name ppx_deriving_hash)
(generate_opam_files true)
(source (github sim642/ppx_deriving_hash))
(authors "Simmo Saan <simmo.saan@gmail.com>")
(maintainers "Simmo Saan <simmo.saan@gmail.com>")
(license MIT)

(package
 (name ppx_deriving_hash)
 (synopsis "[@@deriving hash]")
 (description "Deriver for standard hash functions without extra dependencies.")
 (depends
  ppxlib
  (ppx_deriving (>= 5.0))))
