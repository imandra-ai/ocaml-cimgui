(lang dune 2.0)

(name imgui)
(version 0.1)
(generate_opam_files true)
(maintainers simon@imandra.ai)
(authors "the imgui authors" simon@imandra.ai)
(license MIT)
(source (github aestheticintegration/ocaml-cimgui))
(homepage https://github.com/aestheticintegration/ocaml-cimgui)

(package
  (name imgui)
  (synopsis "Bindings to Dear Imgui")
  (tags (gui bindings imgui "dear imgui"))
  (depends
    (yojson (and :build (>= "1.6")))
    ctypes)
  (depopts tgls tsdl)
  )

(package
  (name imgui-sdl-opengl3)
  (synopsis "Bindings to Dear Imgui with SDL2+openGL3 wrapper")
  (tags (gui bindings imgui "dear imgui"))
  (depends
    (imgui (= :version))
    ctypes
    tsdl
    tgls)
  )


