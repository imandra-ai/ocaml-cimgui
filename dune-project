(lang dune 2.0)

(name imgui)
(version 0.1)
(generate_opam_files true)
(license MIT)
(source (github aestheticintegration/ocaml-cimgui))
(homepage https://github.com/aestheticintegration/ocaml-cimgui)

(package
  (name imgui)
  (synopsis "Bindings to Dear Imgui")
  (tags (gui bindings imgui "dear imgui"))
  (depends
    (yojson (and :build (>= "1.6")))
    (conf-cmake :build)
    ctypes))


