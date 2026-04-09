(lang dune 3.9)

(name ephemeral)

(generate_opam_files true)

(source
 (github CharlesAverill/ephemeral))

(authors "Charles Averill")

(maintainers "Charles Averill")

(license MIT)

(documentation https://github.com/CharlesAverill/ephemeral)

(package
 (name ephemeral)
 (synopsis "Visualize spacecraft trajectories")
 (description "Parser and visualizer for JPL Ephemeris vector tables")
 (depends ocaml dune alcotest ocamlformat odoc graphics calendar ppx_unreachable))

