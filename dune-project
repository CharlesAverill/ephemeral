(lang dune 3.18)

(name ephemeral)

(generate_opam_files true)

(source
 (github CharlesAverill/ephemeral))

(authors "Charles Averill <charlesaverill20@gmail.com>")

(maintainers "Charles Averill <charlesaverill20@gmail.com>")
(maintenance_intent "(latest)")

(license MIT)

(documentation https://charlesaverill.github.io/ephemeral/module/Ephemeral/) 

(package
 (name ephemeral)
 (synopsis "Visualize spacecraft trajectories")
 (description "Parser and visualizer for JPL Ephemeris vector tables")
 (depends 
   (ocaml (>= 4.13.1))
   dune
   graphics
   (calendar (>= 3.0.0))
   ppx_unreachable
   (ocamlformat :with-doc)
   (odoc :with-doc))
 (tags
   ("NASA" "Space" "Spacecraft" "Moon"
    "Plotting" "Trajectory")))

